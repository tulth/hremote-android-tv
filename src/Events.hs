{-# LANGUAGE OverloadedStrings #-}
module Events
  (
    eventCodes
  , eventCodeToEventCmd
  , buttonToEventCode
  ) where

import qualified RIO.Map as M

-- name to eventid to event code mapping
eventCodes :: [(String, Int)]
eventCodes = [ ("dpad_up"    , 103)
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

eventCodeToEventCmd :: String -> Int -> String
eventCodeToEventCmd device eventCode =
  "sendevent " ++ device ++ " 1 " ++ show eventCode ++ " 1 "
  ++ "; sendevent " ++ device ++ " 0 0 0"
  ++ "; sendevent " ++ device ++ " 1 " ++ show eventCode ++ " 0 "
  ++ "; sendevent " ++ device ++ " 0 0 0"

buttonToEventCode :: String -> Maybe Int
buttonToEventCode button = buttonEventcodeMap M.!? button
  where buttonEventcodeMap = M.fromList eventCodes
