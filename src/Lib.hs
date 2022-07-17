{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Lib
    ( mainApp
    , parseMqttLine
    , mqttMsgToEventCode
    , MqttMsg(..)
    , buttonEventcodePairs
    , mqttMsgToEventCmd
    ) where

import System.IO (hGetLine
                 , Handle
                 , hSetBuffering
                 , stdout
                 , stderr
                 , BufferMode(LineBuffering))

import Control.Monad ( forever, join, void, when )

import System.Process.Typed (createPipe
                            , withProcessTerm
                            , withProcessWait_
                            , setStdin
                            , setStdout
                            , setStderr
                            , getStdout
                            , getStdin
                            , closed
                            , shell
                            , ExitCodeException(..)
                            )
import GHC.IO.Exception (ioe_type, IOErrorType(ResourceVanished, NoSuchThing))
import Control.Exception (catch, throw)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_,)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue
                                       , newTBQueueIO
                                       , readTBQueue
                                       , peekTBQueue
                                       , writeTBQueue
                                       )

import MosquittoWrap
import Event
import ParseDumpsys
import Timeout (timeout)
import HandleHelpers (guardEof, hReadUntilNotReady, hPutStrLnWithTimeout)

hPutStrLnTimeout :: Int
hPutStrLnTimeout = 500 * 1000

eventSendTimeout :: Int
eventSendTimeout = 10 * 60 * 1000 * 1000

mqttMsgToEventCmd :: String -> String -> [(String, Int)] -> MqttMsg -> Maybe String
mqttMsgToEventCmd device topicPrefix butEcPairs msg =
  eventCodeToEventCmd device <$> mqttMsgToEventCode topicPrefix butEcPairs msg

mqttMsgAction :: TBQueue Int -> MqttMsg -> IO ()
mqttMsgAction queue msg =
  case eventCodeMaybe of
    Nothing       -> putStrLn $ "Unable to convert mqtt msg to event command: " ++ show msg
    Just eventCode -> do
      putStrLn $ "Sending event: " ++ show eventCode
      atomically $ writeTBQueue queue eventCode
  where topicPrefix = "home/downstairs/shield/button/"
        mqttMsgToEventCode' = mqttMsgToEventCode topicPrefix buttonEventcodePairs
        eventCodeMaybe = mqttMsgToEventCode' msg

lineAction :: TBQueue Int -> String -> IO ()
lineAction queue l =
  maybe
    (putStrLn $ "Failed Parsing line: " ++ l) -- on parse error
    mqttMsgAction'                            -- on success
    (parseMqttLine l)                         -- parse the line produce a maybe
    where mqttMsgAction' = mqttMsgAction queue

lineLoop :: TBQueue Int -> Handle -> IO ()
lineLoop queue mosquittoReadHandle =
  guardEof hGetLine mosquittoReadHandle >>=
  (\case
      Nothing -> putStrLn "hit eof in lineloop"
      Just eventCode -> lineAction queue eventCode >> lineLoop queue mosquittoReadHandle
  )

mqttWatch :: TBQueue Int -> IO ()
mqttWatch queue = do
  forever $ do
    putStrLn $ "connecting to mosquitto with command: " ++ mosquittoCmd
    withProcessTerm mosquittoProcCfg $ \mosquittoProc -> do
      lineLoop queue (getStdout mosquittoProc)
    putStrLn "Lost connection to mosquitto_sub subprocess.  Sleeping and retrying..."
    sleep 10
  where buttons = fst <$> buttonEventcodePairs
        topicPrefix = "home/downstairs/shield/button/"
        topics = (topicPrefix ++) <$> buttons
        mosquittoCmd = makeMqttCmd defaultMqttBroker topics
        mosquittoProcCfg = setStdin createPipe
                  $ setStdout createPipe
                  $ setStderr closed
                  $ shell mosquittoCmd

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

readTBQueueIO :: TBQueue a -> IO a
readTBQueueIO = atomically . readTBQueue

eventSendLoop :: String -> TBQueue Int -> Handle -> IO ()
eventSendLoop inputDev queue outHandle = do
  timeout eventSendTimeout (readTBQueueIO queue)
  >>= (\case
          Nothing -> do
            putStrLn "eventSendLoop: timed out.  Closing adb shell."
            void $ shellPutStrLn "exit"
          Just eventCode -> do
            let cmd = eventCodeToEventCmd inputDev eventCode
            putStrLn $ "adb shell cmd: " ++ cmd
            success <- shellPutStrLn cmd
            when success $ eventSendLoop inputDev queue outHandle
      )
  where shellPutStrLn = hPutStrLnWithTimeout hPutStrLnTimeout outHandle

eventPrintLoop :: Handle -> IO ()
eventPrintLoop inHandle = do
  putStrLn "eventPrintLoop"
  eAdbLine <- guardEof hGetLine inHandle
  case eAdbLine of
    Just adbLine ->
      putStrLn $ "adb shell response: " ++ adbLine
    Nothing -> return ()

-- | Read handle until the string read decodes to usable event devices
getEventDev :: Handle -> IO String
getEventDev = loop ""
  where
    loop :: String -> Handle -> IO String
    loop rxCharAccum rxH = do
      newChars <- hReadUntilNotReady rxH
      let dumpsysInputResult = rxCharAccum ++ newChars
      let evDevs = parseDumpsysGetEventDevs dumpsysInputResult
      case evDevs of
        x : _ -> do
          putStrLn $ "detected evDevs: " ++ show evDevs ++ ", selecting evDev " ++ x
          return x
        _ -> loop dumpsysInputResult rxH

eventLoop
  :: TBQueue Int -- ^ queue of events to send
  -> Handle -- ^ subprocess rx handle (ie stdin)
  -> Handle -- ^ subprocess tx handle (ie stdout)
  -> IO ()
eventLoop queue rxH txH = do
  success <- shellPutStrLn "dumpsys input"
  when success $ do
    mEvDev <- join <$> timeout t3Seconds (guardEof getEventDev rxH)
    maybe (return ())
      (\evDev -> race_
        (eventSendLoop evDev queue txH)
        (eventPrintLoop rxH))
      mEvDev
  where t3Seconds = 3 * 1000 * 1000
        shellPutStrLn = hPutStrLnWithTimeout hPutStrLnTimeout txH

eventProcess :: TBQueue Int -> IO ()
eventProcess queue = do
  putStrLn "eventProcess: waiting until there is an event in the queue"
  _ <- atomically $ peekTBQueue queue  -- wait until something is in the queue
  catch
    ( do
        putStrLn $ "connecting to adb shell with command: " ++ adbConnectCommand
        withProcessWait_ adbProcCfg $ \adbProc ->
          eventLoop queue (getStdout adbProc) (getStdin adbProc)
        putStrLn "eventProcess: adb shell subprocess ended."
    )
    (\e ->
        if ioe_type e == ResourceVanished then do
          restart $ "eventProcess: ResourceVanished: Lost connection to adb shell subprocess."
            ++ "  Sleeping and retrying..."
        -- skip error waitForProcess: does not exist (No child processes)
        else if ioe_type e == NoSuchThing then do
          restart $ "eventProcess: NoSuchThing: Lost connection to adb shell subprocess."
            ++ "  Sleeping and retrying..."
        else do
          print e
          throw e)
    `catch` (\case
                ExitCodeException {} ->
                  restart "Exit code from adb shell subprocess.  Sleeping...")
  eventProcess queue
  where adbConnectCommand = defaultAdbConnectCommand
        adbProcCfg = setStdin createPipe
                     $ setStdout createPipe
                     $ setStderr closed
                     $ shell adbConnectCommand
        restartDelaySec = 10
        restart :: String -> IO ()
        restart msg = do
          putStrLn msg
          sleep restartDelaySec
          eventProcess queue

mainApp :: IO ()
mainApp = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  queue <- newTBQueueIO 8
  forever $ do
    race_
      (mqttWatch queue)
      (eventProcess queue)
    putStrLn "mainApp: eventProcess or mqttWatch died, restarting"
