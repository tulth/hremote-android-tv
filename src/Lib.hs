{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( mainApp
    , parseMqttLine
    , mqttMsgToEventCode
    , MqttMsg(..)
    , buttonEventcodePairs
    , mqttMsgToEventCmd
    ) where

import System.IO (hGetLine, hPutStrLn, Handle)

import System.Time.Extra (sleep)

import Control.Monad ( forever, guard )

import System.Process.Typed
  (
    createPipe
  , withProcessTerm_
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
import Control.Exception (tryJust)
import System.IO.Error (isEOFError)

import MosquittoWrap
import Event

mqttMsgToEventCmd :: String -> String -> [(String, Int)] -> MqttMsg -> Maybe String
mqttMsgToEventCmd device topicPrefix butEcPairs msg =
  eventCodeToEventCmd device <$> mqttMsgToEventCode topicPrefix butEcPairs msg

safeHGetLine :: Handle -> IO (Either () String)
safeHGetLine h = tryJust (guard . isEOFError) (hGetLine h)

mqttMsgAction :: Handle -> MqttMsg -> IO ()
mqttMsgAction eventOutputHandle msg =
  case eventCmdMaybe of
    Nothing       -> putStrLn $ "Unable to convert mqtt msg to event command: " ++ show msg
    Just eventCmd -> do
      putStrLn $ "Executing command: " ++ eventCmd
      hPutStrLn eventOutputHandle eventCmd
  where
    topicPrefix = "home/downstairs/shield/button/"
    mqttMsgToEventCmd' = mqttMsgToEventCmd defaultInputDevice topicPrefix buttonEventcodePairs
    eventCmdMaybe = mqttMsgToEventCmd' msg


lineAction :: Handle -> String -> IO ()
lineAction eventOutputHandle l =
  maybe
    (putStrLn $ "Failed Parsing line: " ++ l) -- on parse error
    mqttMsgAction'           -- on success
    (parseMqttLine l)                      -- parse the line produce a maybe
    where mqttMsgAction' = mqttMsgAction eventOutputHandle

lineLoop :: Handle -> Handle -> Handle -> IO ()
lineLoop mosquittoReadHandle adbWriteHandle adbReadHandle =
  safeHGetLine mosquittoReadHandle >>=
  (\case
      Left err -> print err
      Right l -> lineAction' l >> lineLoop mosquittoReadHandle adbWriteHandle adbReadHandle
  )
  where lineAction' = lineAction adbWriteHandle

myGetExitCode :: Process stdin stdout stderr -> IO (Maybe ExitCode)
myGetExitCode = getExitCode

mainApp :: IO ()
mainApp = forever $ do
  putStrLn $ "connecting to mosquitto with command: " ++ mosquittoCmd
  putStrLn $ "connecting to adb shell with command: " ++ adbConnectCommand
  withProcessTerm_ mosquittoProcCfg $ \mosquittoProc -> do
    withProcessTerm_ adbProcCfg $ \adbProc -> do
      sleep 1
      putStrLn "HERE0"
      mec <- myGetExitCode adbProc
      print mec
      putStrLn "HERE1"
      lineLoop (getStdout mosquittoProc) (getStdin adbProc) (getStdout adbProc)
      putStrLn "HERE2"
    putStrLn "HERE3"
  putStrLn "Lost connection to mosquitto_sub subprocess.  Sleeping..."
  sleep 10
  where buttons = fst <$> buttonEventcodePairs
        topicPrefix = "home/downstairs/shield/button/"
        topics = (topicPrefix ++) <$> buttons
        mosquittoCmd = makeMqttCmd defaultMqttBroker topics
        adbConnectCommand = defaultAdbConnectCommand
        mosquittoProcCfg = setStdin createPipe
                  $ setStdout createPipe
                  $ setStderr closed
                  $ shell mosquittoCmd
        adbProcCfg = setStdin createPipe
                  $ setStdout createPipe
                  $ setStderr closed
                  $ shell adbConnectCommand
