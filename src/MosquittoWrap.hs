module MosquittoWrap
  (
    parseMqttLine
  , mqttMsgToEventCode
  , defaultMqttBroker
  , makeMqttCmd
  , MqttMsg(..)
  , MqttTopic
  , MqttPayload
  ) where
  
import Data.List.Extra
  (
    lower
  )

import qualified Data.Map as M

type MqttTopic = String
type MqttPayload = String
data MqttMsg = MqttMsg MqttTopic MqttPayload deriving (Show, Eq)

defaultMqttBroker :: String
defaultMqttBroker = "mqtt-broker"

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
          let payload = unwords $ payloadStart : drop 3 parts
          return (msgId, topic, payload)

mqttMsgToEventCode :: String -> [(String, Int)] -> MqttMsg -> Maybe Int
mqttMsgToEventCode topicPrefix buttonEventcodePairs (MqttMsg topic payload) =
  if isPayloadOn (lower payload)
  then topicEventcodeMap M.!? topic
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

makeMqttCmd :: String -> [String] -> String
makeMqttCmd host topics =
  "mosquitto_sub -F \"MSG %t %p\" -h " ++ host ++ topicsToArgs topics
  where topicsToArgs :: [String] -> String
        topicsToArgs = concatMap (" -t " ++) 


-- nice helper that is missing from data list extra in some versions
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
                
