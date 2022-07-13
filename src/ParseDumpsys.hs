{-# LANGUAGE LambdaCase #-}
module ParseDumpsys
    ( parseDumpsysGetEventDevs
    , topicTest
    , parseClass
    , isAcceptableClass
    , Cat(..)
    ) where

import Text.Read (readMaybe)
import Control.Applicative ((<$>))
import Data.List (isPrefixOf, sort)
import Data.Functor ((<&>))
import Data.Bits ((.&.))

-- import qualified Data.Map as M
data Cat = Cat String [Cat] deriving Show

-- newtype Cat2 = Cat2 (M.Map String [Cat2]) deriving Show

defaultIndentLevel :: Int
defaultIndentLevel = 4 -- leading spaces

level :: Int -> String -> Int
level l = (`div` l) . length . takeWhile (== ' ')

parse :: Int -> [String] -> [Cat]
parse l [] = []
parse l (x : xs) = Cat (trim x) (parse l children) : parse l sisters
    where
        level' = level l
        (children, sisters) = span ((level' x <) . level') xs
        trim                = dropWhile (== ' ')

parseTop :: Int -> String -> [Cat]
parseTop l = parse l . filter (/= "") . lines

getTopic :: Cat -> String
getTopic = \case Cat topic _ -> topic

topicTest :: (String -> Bool) -> Cat -> Bool
topicTest test = test . getTopic

parseClass :: String -> Maybe Int
parseClass s =
  if "Classes: " `isPrefixOf` s
  then readMaybe (drop (length "Classes: ") s)
  else Nothing

isAcceptableClass :: String -> Bool
isAcceptableClass =
  isAcceptableClass' . parseClass
  where isAcceptableClass' :: Maybe Int -> Bool
        isAcceptableClass' = \case
          Just x -> (x .&. 0x21) == 0x21
          _ -> False

getDevices :: [Cat] -> [Cat]
getDevices dump =
  dump
  >>= (\case
            Cat "Event Hub State:" x -> x
            _ -> []
        )
  >>= (\case
          Cat "Devices:" x -> x
          _ -> []
      )
  >>= (\l -> case l of
          Cat _ children -> [l | any (topicTest (isPrefixOf "Path: /dev/input/event")) children]
      )
  >>= (\l -> case l of
          Cat _ children -> [l | any (topicTest isAcceptableClass) children]
      )

getAcceptableEventDevs :: [Cat] -> [String]
getAcceptableEventDevs devs = sort $
  devs
  >>= (\(Cat _ children) -> filter (topicTest (isPrefixOf "Path: ")) children)
  <&> getTopic
  <&> drop (length "Path: ")
  where stripPathPrefix :: String -> String
        stripPathPrefix = drop (length "Path: ")

parseDumpsysGetEventDevs' :: Int -> String -> [String]
parseDumpsysGetEventDevs' l s = 
  getAcceptableEventDevs $ getDevices $ parseTop l s

parseDumpsysGetEventDevs :: String -> [String]
parseDumpsysGetEventDevs = parseDumpsysGetEventDevs' 2
