{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( mainApp
    , parseMqttLine
    , mqttMsgToEventCode
    , MqttMsg(..)
    , buttonEventcodePairs
    , topicizeButtonEventcodePairs
    , mqttMsgToEventCmd
    ) where

import System.IO

import Data.Maybe
import Data.Functor
import Data.List.Extra

import qualified Data.Map as M

import Control.Monad

import Data.ByteString.Lazy (ByteString)

import System.Process.Typed

defaultMqttBroker = "mqtt-broker"
defaultInputDevice = "/dev/input/event3"
defaultAdbConnectCommand = "adb connect nvidia-shield-tv ; adb shell"
  
buttonEventcodePairs :: [(String, Int)]
buttonEventcodePairs = [ ("dpad_up"    , 103)
                       , ("dpad_down"  , 108)
                       , ("dpad_left"  , 105)
                       , ("dpad_right" , 106)
                       , ("dpad_center", 28 )
                       , ("back"       , 1  )
                       , ("home"       , 172)
                       , ("menu"       , 139)
                       , ("volume_up"  , 115)
                       , ("volume_down", 114)
                       , ("a"          , 304)
                       , ("b"          , 305)
                       , ("x"          , 307)
                       , ("y"          , 308)
                       , ("l1"         , 310)
                       , ("r1"         , 311)
                       , ("rtrigger"   , 312)
                       , ("ltrigger"   , 313)
                       , ("select"     , 314)
                       , ("start"      , 315)]
  
makeMqttCmd :: String -> [String] -> String
makeMqttCmd host topics =
  "mosquitto_sub -F \"MSG %t %p\" -h " ++ host ++ (topicsToArgs topics)
  where topicsToArgs :: [String] -> String
        topicsToArgs topics = concat (map (\t -> " -t " ++ t) topics)


eventCodeToEventCmd :: String -> Int -> String
eventCodeToEventCmd device eventCode =
  "sendevent " ++ device ++ " 1 " ++ (show eventCode) ++ " 1 "
  ++ "; sendevent " ++ device ++ " 0 0 0"
  ++ "; sendevent " ++ device ++ " 1 " ++ (show eventCode) ++ " 0 "
  ++ "; sendevent " ++ device ++ " 0 0 0"
  ++ "\n"

type MqttTopic = String
type MqttPayload = String

data MqttMsg = MqttMsg MqttTopic MqttPayload deriving (Show, Eq)

parseMqttLine :: String -> Maybe MqttMsg
parseMqttLine l =
  msgTopicPayload >>=
    \(msgId, topic, payload) -> if msgId == "MSG"
                                then Just $ MqttMsg topic payload
                                else Nothing
  where parts = words l
        msgTopicPayload :: Maybe (String, MqttTopic, MqttPayload)
        msgTopicPayload = do
          msgId <- parts !? 0
          topic <- parts !? 1
          payloadStart <- parts !? 2  -- require some payload
          let payload = unwords $ [payloadStart] ++ (drop 3) parts
          return (msgId, topic, payload)

mqttMsgToEventCode :: String -> [(String, Int)] -> MqttMsg -> Maybe Int
mqttMsgToEventCode topicPrefix butEcPairs (MqttMsg topic payload) =
  if isPayloadOn (lower payload)
  then (topicEventcodeMap M.!? topic)
  else Nothing
  where topicEventcodePairs = topicizeButtonEventcodePairs topicPrefix buttonEventcodePairs
        topicEventcodeMap = M.fromList topicEventcodePairs
        isPayloadOn :: MqttPayload -> Bool
        isPayloadOn "1" = True
        isPayloadOn "on" = True
        isPayloadOn "buttonpress" = True
        isPayloadOn "active" = True
        isPayloadOn "activate" = True
        isPayloadOn _ = False
  
topicizeButtonEventcodePairs :: String -> [(String, Int)] -> [(String, Int)]
topicizeButtonEventcodePairs topicPrefix buttonEventCodePairs =
  topicizeButtonEventcodePair <$> buttonEventCodePairs
  where topicizeButtonEventcodePair :: (String, Int) -> (String, Int)
        topicizeButtonEventcodePair (t, e) = (topicPrefix ++ t, e)
        
mqttMsgToEventCmd :: String -> String -> [(String, Int)] -> MqttMsg -> Maybe String
mqttMsgToEventCmd device topicPrefix butEcPairs msg =
  (eventCodeToEventCmd device) <$> (mqttMsgToEventCode topicPrefix butEcPairs msg)

mainApp :: IO ()
mainApp = do
  let buttons = (fst <$> buttonEventcodePairs)
  let buttonTopics = (\b -> topicPrefix ++ b) <$> buttons
  let topics = buttonTopics
  let procCfg = setStdin createPipe
                $ setStdout createPipe
                $ setStderr closed
                $ shell (makeMqttCmd defaultMqttBroker topics)
  withProcessTerm_ procCfg $ \p -> do
    forever $ hGetLine (getStdout p) >>=
      (\l -> print $ mqttMsgToEventCmd' =<< parseMqttLine l)
                                            

  putStrLn "Lost connection to subprocess"
  where topicPrefix = "home/downstairs/shield/button/"
        mqttMsgToEventCmd' = mqttMsgToEventCmd defaultInputDevice topicPrefix buttonEventcodePairs
