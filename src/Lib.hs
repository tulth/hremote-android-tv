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

import System.IO (hGetLine, hGetChar, hReady, hPutStrLn, hFlush, Handle)

import System.Time.Extra (sleep)

import Control.Monad ( forever, guard, when )

import System.Process.Typed
  (
    createPipe
  , withProcessTerm_
  , withProcessTerm
  , getExitCode
  , ExitCode
  , Process
  , setStdin
  , setStdout
  , setStderr
  , getStdout
  , getStdin
  , closed
  , shell
  )
import Control.Exception (tryJust, finally, catch, throw
                         , SomeException)
import System.IO.Error (isEOFError)
import GHC.IO.Exception (ioe_type, IOErrorType(ResourceVanished))

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMQueue (TBMQueue
                                       , newTBMQueueIO
                                       , readTBMQueue
                                       , writeTBMQueue
                                       , closeTBMQueue
                                       )

import System.Posix (Handler(Ignore), installHandler, sigPIPE)
import MosquittoWrap
import Event

mqttMsgToEventCmd :: String -> String -> [(String, Int)] -> MqttMsg -> Maybe String
mqttMsgToEventCmd device topicPrefix butEcPairs msg =
  eventCodeToEventCmd device <$> mqttMsgToEventCode topicPrefix butEcPairs msg

mqttMsgAction :: TBMQueue Int -> MqttMsg -> IO ()
mqttMsgAction queue msg =
  case eventCodeMaybe of
    Nothing       -> putStrLn $ "Unable to convert mqtt msg to event command: " ++ show msg
    Just eventCode -> do
      putStrLn $ "Sending event: " ++ show eventCode
      atomically $ writeTBMQueue queue eventCode
  where topicPrefix = "home/downstairs/shield/button/"
        mqttMsgToEventCode' = mqttMsgToEventCode topicPrefix buttonEventcodePairs
        eventCodeMaybe = mqttMsgToEventCode' msg

lineAction :: TBMQueue Int -> String -> IO ()
lineAction queue l =
  maybe
    (putStrLn $ "Failed Parsing line: " ++ l) -- on parse error
    mqttMsgAction'                            -- on success
    (parseMqttLine l)                         -- parse the line produce a maybe
    where mqttMsgAction' = mqttMsgAction queue

safeHGetLine :: Handle -> IO (Either () String)
safeHGetLine h = tryJust (guard . isEOFError) (hGetLine h)

lineLoop :: TBMQueue Int -> Handle -> IO ()
lineLoop queue mosquittoReadHandle =
  safeHGetLine mosquittoReadHandle >>=
  (\case
      Left err -> putStrLn "hit eof in lineloop"
      Right eventCode -> lineAction queue eventCode >> lineLoop queue mosquittoReadHandle
  )

mqttWatch :: TBMQueue Int -> IO ()
mqttWatch queue = do
  forever $ do
    putStrLn $ "connecting to mosquitto with command: " ++ mosquittoCmd
    withProcessTerm mosquittoProcCfg $ \mosquittoProc -> do
      lineLoop queue (getStdout mosquittoProc)
    putStrLn "Lost connection to mosquitto_sub subprocess.  Sleeping..."
    sleep 10
  where buttons = fst <$> buttonEventcodePairs
        topicPrefix = "home/downstairs/shield/button/"
        topics = (topicPrefix ++) <$> buttons
        mosquittoCmd = makeMqttCmd defaultMqttBroker topics
        mosquittoProcCfg = setStdin createPipe
                  $ setStdout createPipe
                  $ setStderr closed
                  $ shell mosquittoCmd

eventSendLoop :: TBMQueue Int -> Handle -> IO ()
eventSendLoop queue outHandle = forever $ do
--  pendingChars <- eventPendingRead inHandle ""
--  when (pendingChars /= "") $ putStr pendingChars
  mEventCode <- atomically $ readTBMQueue queue
  case mEventCode of
    Nothing -> return ()
    Just eventCode -> do
      let cmd = eventCodeToEventCmd inputDevice eventCode
      putStrLn $ "adb shell cmd: " ++ cmd
      hPutStrLn outHandle cmd
      hFlush outHandle
  where inputDevice = defaultInputDevice

-- eventPrintLoop :: Handle -> IO ()
-- eventPrintLoop inHandle = forever $ do
--   adbLine <- hGetLine inHandle
--   putStrLn $ "adb shell reponse: " ++ adbLine

eventPrintLoop :: Handle -> IO ()
eventPrintLoop inHandle = forever $ do
  eAdbLine <- safeHGetLine inHandle
  case eAdbLine of
    Right adbLine ->
      putStrLn $ "adb shell reponse: " ++ adbLine
    Left _ -> return ()

-- safeEventPendingRead :: Handle -> String -> IO (String)
-- safeHGetChar :: Handle -> IO (Either () String)
-- safeHGetChar h = tryJust (guard . isEOFError) (hGetChar h)

-- eventPendingRead :: Handle -> String -> IO String
-- eventPendingRead inHandle pendingChars = do
--   hasAChar <- hReady inHandle
--   if hasAChar then do
--     inChar <- hGetChar inHandle
--     eventPendingRead inHandle (pendingChars ++ [inChar])
--   else
--     return pendingChars

eventProcess :: TBMQueue Int -> IO ()
eventProcess queue =
  catch (
  do
    putStrLn $ "connecting to adb shell with command: " ++ adbConnectCommand
    withProcessTerm adbProcCfg $ \adbProc ->
      concurrently_ 
        (eventSendLoop queue (getStdin adbProc))
        (eventPrintLoop (getStdout adbProc)))
  (\e -> if ioe_type e == ResourceVanished
         then do
               putStrLn "Lost connection to adb shell subprocess.  Sleeping..."
               sleep 10
               eventProcess queue
         else throw e)
    where adbConnectCommand = defaultAdbConnectCommand
          adbProcCfg = setStdin createPipe
                       $ setStdout createPipe
                       $ setStderr closed
                       $ shell adbConnectCommand
        -- concurrently_
          -- (eventSendLoop queue (getStdin adbProc))
          -- (eventPrintLoop (getStdout adbProc)))

mainApp :: IO ()
mainApp = do
  installHandler sigPIPE Ignore Nothing
  queue <- newTBMQueueIO 16
  concurrently_
    (mqttWatch queue `finally` atomically (closeTBMQueue queue))
    (catch (eventProcess queue)
     (\e -> if ioe_type e == ResourceVanished then eventProcess queue else
         throw e))
     --(eventProcess queue)

-- mainApp :: IO ()
-- mainApp = forever $ do
--   putStrLn $ "connecting to mosquitto with command: " ++ mosquittoCmd
--   putStrLn $ "connecting to adb shell with command: " ++ adbConnectCommand
--   withProcessTerm_ mosquittoProcCfg $ \mosquittoProc -> do
--     withProcessTerm_ adbProcCfg $ \adbProc -> do
--       sleep 1
--       putStrLn "HERE0"
--       mec <- myGetExitCode adbProc
--       print mec
--       putStrLn "HERE1"
--       lineLoop (getStdout mosquittoProc) (getStdin adbProc) (getStdout adbProc)
--       putStrLn "HERE2"
--     putStrLn "HERE3"
--   putStrLn "Lost connection to mosquitto_sub subprocess.  Sleeping..."
--   sleep 10
--   where buttons = fst <$> buttonEventcodePairs
--         topicPrefix = "home/downstairs/shield/button/"
--         topics = (topicPrefix ++) <$> buttons
--         mosquittoCmd = makeMqttCmd defaultMqttBroker topics
--         adbConnectCommand = defaultAdbConnectCommand
--         mosquittoProcCfg = setStdin createPipe
--                   $ setStdout createPipe
--                   $ setStderr closed
--                   $ shell mosquittoCmd
--         adbProcCfg = setStdin createPipe
--                   $ setStdout createPipe
--                   $ setStderr closed
--                   $ shell adbConnectCommand
