{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Run
  (
    mqttLoop
  , eventSendLoop
  , mqttMsgToEventCode
  , mqttMsgToEventCmd
  ) where

import RIO
import qualified RIO.Map as M
import qualified Data.ByteString.Char8 as BC

import Types
import Util (sleep)
import MqttSub (mqttReceiveAndAct, MqttMsg(..), isMqttMsgPayloadOn)
import Events (eventCodes, eventCodeToEventCmd)
import EventSend (eventSendLoop)

mqttMsgAction
  :: (HasLogFunc env, MonadReader env m, MonadIO m)
  => TBQueue Int -> MqttMsg -> m ()
mqttMsgAction queue msg =
  case eventCodeMaybe of
    Nothing        -> logInfo $ "Unable to convert mqtt msg to event command: " <> fromString (show msg)
    Just eventCode -> do
      logInfo $ "Queueing event: " <> display eventCode
      atomically $ writeTBQueue queue eventCode
  where topicPrefix = "home/downstairs/shield/button/"
        mqttMsgToEventCode' = mqttMsgToEventCode topicPrefix eventCodes
        eventCodeMaybe = mqttMsgToEventCode' msg

mqttMsgToEventCode :: String -> [(String, Int)] -> MqttMsg -> Maybe Int
mqttMsgToEventCode topicPrefix buttonEvPairs (MqttMsg topic payload) =
  if isMqttMsgPayloadOn (MqttMsg topic payload)
  then topicEventcodeMap M.!? BC.unpack topic
  else Nothing
  where topicEventcodePairs = topicizeButtonEventcodePairs topicPrefix buttonEvPairs
        topicEventcodeMap = M.fromList topicEventcodePairs

topicizeButtonEventcodePairs :: String -> [(String, Int)] -> [(String, Int)]
topicizeButtonEventcodePairs topicPrefix buttonEventCodePairs =
  topicizeButtonEventcodePair <$> buttonEventCodePairs
  where topicizeButtonEventcodePair :: (String, Int) -> (String, Int)
        topicizeButtonEventcodePair (t, e) = (topicPrefix ++ t, e)

mqttLoop :: TBQueue Int -> RIO App ()
mqttLoop queue = do
  mqttBroker <- getMqttBroker <$> view optionsL
  forever $ do
    mqttReceiveAndAct mqttBroker topics (mqttMsgAction queue)
    logInfo "Lost connection to mqtt_sub subprocess.  Sleeping and retrying..."
    sleep 10
  where buttons = fst <$> eventCodes
        topicPrefix = "home/downstairs/shield/button/"
        topics = (topicPrefix ++) <$> buttons


mqttMsgToEventCmd :: String -> String -> [(String, Int)] -> MqttMsg -> Maybe String
mqttMsgToEventCmd device topicPrefix butEcPairs msg =
  eventCodeToEventCmd device <$> mqttMsgToEventCode topicPrefix butEcPairs msg

