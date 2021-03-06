{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (isNothing)

import ParseDumpsys
import Data.List (isPrefixOf)
import Run (mqttMsgToEventCode, mqttMsgToEventCmd)
import MqttSub
import Events (eventCodes)

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

parseMqttLineTest :: Bool
parseMqttLineTest =
  (parseMqttLine "MSG home/downstairs/shield/button/dpad_up buttonPress" ==
   Just (MqttMsg "home/downstairs/shield/button/dpad_up" "buttonPress"))
  && (parseMqttLine "MSG home/downstairs/shield/button/back On" ==
      Just (MqttMsg "home/downstairs/shield/button/back" "On"))
  && (parseMqttLine "MSG home/downstairs/shield/button/back 3 Word Payload" ==
      Just (MqttMsg "home/downstairs/shield/button/back" "3 Word Payload"))
  && isNothing (parseMqttLine "")
  && isNothing (parseMqttLine "M home/downstairs/shield/button/back On")
  && isNothing (parseMqttLine "home/downstairs/shield/button/back On")
  && isNothing (parseMqttLine "home/downstairs/shield/button/back")

mqttMsgToEventCodeTest :: Bool
mqttMsgToEventCodeTest =
  (mqttMsgToEventCode' (MqttMsg "home/downstairs/shield/button/back" "On") == Just 1)
  && (mqttMsgToEventCode' (MqttMsg "home/downstairs/shield/button/dpad_up" "ON") == Just 103)
  && (mqttMsgToEventCode' (MqttMsg "home/downstairs/shield/button/dpad_down" "on") == Just 108)
  && (mqttMsgToEventCode' (MqttMsg "home/downstairs/shield/button/home" "buttonPress") == Just 172)
  && (mqttMsgToEventCode' (MqttMsg "home/downstairs/shield/button/home" "1") == Just 172)
  && isNothing (mqttMsgToEventCode' (MqttMsg "home/downstairs/shield/button/home" "Off"))
  && isNothing (mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/home" "0")
  && isNothing (mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/should_fail" "On")
  && isNothing (mqttMsgToEventCode' (MqttMsg "should_fail2" "On"))
  where mqttMsgToEventCode' = mqttMsgToEventCode "home/downstairs/shield/button/" eventCodes

mqttMsgToEventCmd' :: MqttMsg -> Maybe String
mqttMsgToEventCmd' = mqttMsgToEventCmd "/dev/input/event3" "home/downstairs/shield/button/" eventCodes

mqttMsgToEventCmdTest :: Bool
mqttMsgToEventCmdTest =
  (mqttMsgToEventCmd' (MqttMsg "home/downstairs/shield/button/home" "buttonPress") ==
    Just ("sendevent /dev/input/event3 1 172 1 ; "
       ++ "sendevent /dev/input/event3 0 0 0; "
       ++ "sendevent /dev/input/event3 1 172 0 ; "
       ++ "sendevent /dev/input/event3 0 0 0"))
  && isNothing (mqttMsgToEventCmd' $ MqttMsg "home/downstairs/shield/button/home" "0")

parseDumpsysGetEventDevsTest' :: String -> Bool
parseDumpsysGetEventDevsTest' fileContent =
  (parseDumpsysGetEventDevs fileContent ==
    ["/dev/input/event2"
    , "/dev/input/event3"
    , "/dev/input/event4"
    , "/dev/input/event5"
    , "/dev/input/event6"
    , "/dev/input/event7"])
  && null (parseDumpsysGetEventDevs partialInsufficientFileContent0)
  && null (parseDumpsysGetEventDevs partialInsufficientFileContent1)
  && null (parseDumpsysGetEventDevs partialInsufficientFileContent2)
  && null (parseDumpsysGetEventDevs partialInsufficientFileContent3)
  && (parseDumpsysGetEventDevs partialSufficientFileContent0 ==
    ["/dev/input/event3"])
  && (parseDumpsysGetEventDevs partialSufficientFileContent1 ==
    ["/dev/input/event3", "/dev/input/event5"])
  && (parseDumpsysGetEventDevs partialSufficientFileContent2 ==
    ["/dev/input/event3", "/dev/input/event5", "/dev/input/event6"])
  where partialInsufficientFileContent0 = []
        partialInsufficientFileContent1 = unlines $ take 1 $ lines fileContent
        partialInsufficientFileContent2 = unlines $ take 30 $ lines fileContent
        partialInsufficientFileContent3 = unlines $ take 68 $ lines fileContent
        partialSufficientFileContent0 = unlines $ take 69 $ lines fileContent
        partialSufficientFileContent1 = unlines $ take 82 $ lines fileContent
        partialSufficientFileContent2 = unlines $ take 95 $ lines fileContent

parseDumpsysGetEventDevsTest :: IO (Bool)
parseDumpsysGetEventDevsTest =
  parseDumpsysGetEventDevsTest' <$> readFile "dumpsys.txt"

topicTestTest :: Bool
topicTestTest =
  topicTest (== "what") (Cat "what" [])
  && not (topicTest (== "nope") (Cat "what" []))
  && topicTest (isPrefixOf "wh") (Cat "what" [])
  && not (topicTest (isPrefixOf "no") (Cat "what" []))

isAcceptableClassTest :: Bool
isAcceptableClassTest =
  isAcceptableClass "Classes: 0x00000021"
  && isAcceptableClass "Classes: 0x00000023"
  && not (isAcceptableClass "Classes: 0x00000020")
  && not (isAcceptableClass "Classes: 0x80000000")
  && not (isAcceptableClass "nope")

main :: IO ()
main = do
  putStrLn "Running tests..."
  assert parseMqttLineTest "passed 'parseMqttLineTest'" "failed 'parseMqttLineTest'"
  assert mqttMsgToEventCodeTest "passed 'mqttMsgToEventCodeTest'" "failed 'mqttMsgToEventCodeTest'"
  assert mqttMsgToEventCmdTest "passed 'mqttMsgToEventCmdTest'" "failed 'mqttMsgToEventCmdTest'"
  assert topicTestTest "passed 'topicTestTest'" "failed 'topicTestTest'"
  assert isAcceptableClassTest "passed 'isAcceptableClassTest'" "failed 'isAcceptableClassTest'"
  parseDumpsysGetEventDevsTestVal <- parseDumpsysGetEventDevsTest
  assert parseDumpsysGetEventDevsTestVal "passed 'parseDumpsysGetEventDevsTest'" "failed 'parseDumpsysGetEventDevsTest'"
  putStrLn "done!"

