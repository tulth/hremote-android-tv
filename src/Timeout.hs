{-# LANGUAGE LambdaCase #-}

-- simple timeout using concurrent async 

module Timeout
  (timeout) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)

timeout :: Int -- ^ timeout in microseconds
  -> IO a -- ^ function to run for upto timeout time
  -> IO (Maybe a) 
timeout maxTime f =
  (\case
    Right v ->  Just v
    Left _ -> Nothing)
  <$> race (threadDelay maxTime) f

