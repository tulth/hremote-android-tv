{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module MqttSub
  (
    mqttReceiveAndAct
  , MqttMsg(..)
  , MqttPayload
  , MqttTopic
  , isMqttMsgPayloadOn
  , parseMqttLine
  ) where

import RIO
import RIO.Process
import qualified RIO.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.List.Extra (lower)

import Shell
import Util (guardEof, (!?))

type MqttTopic = ByteString
type MqttPayload = ByteString
data MqttMsg = MqttMsg MqttTopic MqttPayload deriving (Show, Eq)

isMqttMsgPayloadOn :: MqttMsg -> Bool
isMqttMsgPayloadOn (MqttMsg _ payload) =
  isPayloadOn $ lower $ BC.unpack payload
  where isPayloadOn "1" = True
        isPayloadOn "on" = True
        isPayloadOn "buttonpress" = True
        isPayloadOn "active" = True
        isPayloadOn "activate" = True
        isPayloadOn _ = False

mqttReceiveAndAct
  :: (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  => String -- ^ mqtt broker host name
  -> [String] -- ^ mqtt topics to watch
  -> (MqttMsg -> m ()) -- ^action to take on each mqtt msg received
  -> m ()
mqttReceiveAndAct mqttBroker mqttTopics msgAction = do
  logInfo $ "connecting to mqtt with command: " <> fromString mqttCmd
  shell mqttCmd $ \processConfig ->
    let customProcessConfig =
          setCreateGroup True $
          setNewSession True $
          setStdin createPipe $
          setStdout createPipe $
          setStderr closed processConfig
     in withProcessWait customProcessConfig $ \process ->
                                            lineLoop msgAction (getStdout process)
  where mqttCmd = makeMqttCmd mqttBroker mqttTopics

lineLoop
  :: (HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  => (MqttMsg -> m ()) -> Handle -> m ()
lineLoop mqttMsgAction mosquittoReadHandle =
  guardEof B.hGetLine mosquittoReadHandle >>=
  (\case
      Nothing -> logInfo "hit eof in lineloop"
      Just eventCode -> lineAction mqttMsgAction eventCode >> lineLoop mqttMsgAction mosquittoReadHandle
  )

lineAction
  :: (HasLogFunc env, MonadReader env m, MonadIO m)
  => (MqttMsg -> m ()) -> ByteString -> m ()
lineAction mqttMsgAction l = do
  logDebug $ displayBytesUtf8 l
  logDebug $ fromString $ show $ BC.words l
  maybe
    (logError $ "Failed Parsing line: " <> displayBytesUtf8 l) -- on parse error
    mqttMsgAction                             -- on success
    (parseMqttLine l)                         -- parse the line produce a maybe

parseMqttLine :: ByteString -> Maybe MqttMsg
parseMqttLine l =
  msgTopicPayload >>=
    \(msgId, topic, payload) -> if msgId == "MSG"
                                then Just $ MqttMsg topic payload
                                else Nothing
  where parts :: [ByteString]
        parts = BC.words l
        msgTopicPayload :: Maybe (ByteString, MqttTopic, MqttPayload)
        msgTopicPayload = do
          msgId <- parts !? 0
          topic <- parts !? 1
          payloadStart <- parts !? 2  -- require some payload
          let payload = BC.unwords $ payloadStart : drop 3 parts
          return (msgId, topic, payload)

makeMqttCmd :: String -> [String] -> String
makeMqttCmd host topics =
  "mosquitto_sub -F \"MSG %t %p\" -h " ++ host ++ topicsToArgs topics
  where topicsToArgs :: [String] -> String
        topicsToArgs = concatMap (" -t " ++)
