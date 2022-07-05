import Lib

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

parseMqttLineTest :: Bool
parseMqttLineTest =
  True
  && ((parseMqttLine "MSG home/downstairs/shield/button/dpad_up buttonPress") ==
   (Just $ MqttMsg "home/downstairs/shield/button/dpad_up" "buttonPress"))
  && ((parseMqttLine "MSG home/downstairs/shield/button/back On") ==
      (Just $ MqttMsg "home/downstairs/shield/button/back" "On"))
  && ((parseMqttLine "MSG home/downstairs/shield/button/back 3 Word Payload") ==
      (Just $ MqttMsg "home/downstairs/shield/button/back" "3 Word Payload"))
  && ((parseMqttLine "") == Nothing)
  && ((parseMqttLine "M home/downstairs/shield/button/back On") == Nothing)
  && ((parseMqttLine "home/downstairs/shield/button/back On") == Nothing)
  && ((parseMqttLine "home/downstairs/shield/button/back") == Nothing)

mqttMsgToEventCodeTest :: Bool
mqttMsgToEventCodeTest =
  True
  && ((mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/back" "On") == Just 1)
  && ((mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/dpad_up" "ON") == Just 103)
  && ((mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/dpad_down" "on") == Just 108)
  && ((mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/home" "buttonPress") == Just 172)
  && ((mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/home" "1") == Just 172)
  && ((mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/home" "Off") == Nothing)
  && ((mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/home" "0") == Nothing)
  && ((mqttMsgToEventCode' $ MqttMsg "home/downstairs/shield/button/should_fail" "On") == Nothing)
  && ((mqttMsgToEventCode' $ MqttMsg "should_fail2" "On") == Nothing)
  where mqttMsgToEventCode' = mqttMsgToEventCode "home/downstairs/shield/button/" buttonEventcodePairs

mqttMsgToEventCmdTest :: Bool
mqttMsgToEventCmdTest =
  True
  && ((mqttMsgToEventCmd' $ MqttMsg "home/downstairs/shield/button/home" "buttonPress") ==
      (Just "sendevent /dev/input/event3 1 172 1 ; sendevent /dev/input/event3 0 0 0; sendevent /dev/input/event3 1 172 0 ; sendevent /dev/input/event3 0 0 0\n"))
  && ((mqttMsgToEventCmd' $ MqttMsg "home/downstairs/shield/button/home" "0") == Nothing)
  where mqttMsgToEventCmd' = mqttMsgToEventCmd "/dev/input/event3" "home/downstairs/shield/button/" buttonEventcodePairs

main :: IO ()
main = do
  putStrLn "Running tests..."
  assert parseMqttLineTest "passed 'parseMqttLineTest'" "failed 'parseMqttLineTest'"
  assert mqttMsgToEventCodeTest "passed 'mqttMsgToEventCodeTest'" "failed 'mqttMsgToEventCodeTest'"
  assert mqttMsgToEventCmdTest "passed 'mqttMsgToEventCmdTest'" "failed 'mqttMsgToEventCmdTest'"
  putStrLn "done!"

