{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module EventSend
  (
    eventSendLoop
  ) where

import RIO
import RIO.Process
import qualified Data.ByteString.Char8 as BC

import AdbShell (withAdbShell, adbShellCatch)
import Util (sleep, hReadUntilNotReady, guardEof)
import ParseDumpsys (parseDumpsysGetEventDevs)
import Events (eventCodeToEventCmd)

eventSendLoop
  :: (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  => TBQueue Int
  -> m ()
eventSendLoop queue = do
  logInfo "eventSendLoop: waiting until there is an event in the queue"
  _ <- atomically $ peekTBQueue queue  -- wait until something is in the queue
  adbShellCatch (withAdbShell "nvidia-shield-tv" $ adbShellClient queue)
    (restart "Lost connection to adb shell subprocess.")
    (restart "NoSuchThing: Lost connection to adb shell subprocess.")
    (restart "Exit code from adb shell subprocess.")
  loop
  where loop = eventSendLoop queue
        restart msg = do
          logError $ fromString $ "eventSendLoop: " ++ msg ++ "  Sleeping and retrying..."
          sleep 10
          loop

adbShellClient
  :: (HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  => TBQueue Int
  -> Process Handle Handle ()
  -> m ()
adbShellClient queue adbShellProc = do
  liftIO $ BC.hPutStrLn txH "dumpsys input"
  hFlush txH
  mEvDev <- join <$> timeout t3Seconds (guardEof getEventDev rxH)
  maybe (return ())
    (\evDev -> race_
      (adbShellSendLoop evDev queue txH)
      (adbShellPrintLoop rxH)
    )
    mEvDev
  -- forever $ do
  --   eventCode <- (atomically . readTBQueue) queue
  --   logInfo $ fromString $ "Sending event: " ++ show eventCode
  where t3Seconds = 3 * 1000 * 1000
        rxH = getStdout adbShellProc
        txH = getStdin adbShellProc

-- | Read handle until the string read decodes to usable event devices
getEventDev
  :: (HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  =>  Handle
  -> m String
getEventDev = loop ""
  where
    loop rxCharAccum rxH = do
      newChars <- hReadUntilNotReady rxH
      let dumpsysInputResult = rxCharAccum <> BC.unpack newChars
      let evDevs = parseDumpsysGetEventDevs dumpsysInputResult
      case evDevs of
        x : _ -> do
          logInfo $ fromString $ "detected evDevs: " ++ show evDevs ++ ", selecting evDev " ++ x
          return x
        _ -> loop dumpsysInputResult rxH

adbShellSendLoop
  :: (HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  => String -> TBQueue Int -> Handle -> m ()
adbShellSendLoop inputDev queue outHandle = do
  timeout eventSendTimeout $ (atomically . readTBQueue) queue
  >>= (\case
          Nothing -> do
            logInfo "adbShellSendLoop timed out"
            liftIO $ BC.hPutStrLn outHandle "exit"
            hFlush outHandle
            threadDelay 100000
          Just eventCode -> do
            let cmd = BC.pack $ eventCodeToEventCmd inputDev eventCode
            logInfo $ displayBytesUtf8 $ "adb shell cmd: " `BC.append` cmd
            liftIO $ BC.hPutStrLn outHandle cmd
            hFlush outHandle
            adbShellSendLoop inputDev queue outHandle
      )

eventSendTimeout :: Int
eventSendTimeout = 30 * 1000 * 1000

adbShellPrintLoop
  :: (HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  =>  Handle
  -> m ()
adbShellPrintLoop inHandle = do
  eAdbLine <- guardEof (liftIO . BC.hGetLine) inHandle
  case eAdbLine of
    Just adbLine ->
      logInfo $ displayBytesUtf8 $ BC.append "adb shell response: " adbLine
    Nothing -> do
      logInfo "adbShellPrintLoop: hit EOF on shell - returning"
      return ()
