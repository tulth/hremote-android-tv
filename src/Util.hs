{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Util
  ( plus2
  , guardEof
  , (!?)
  , sleep
  , hReadUntilNotReady
  , hPutStrLnWithTimeout
  ) where

import RIO
import qualified RIO.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.Either.Extra (eitherToMaybe)

import System.IO.Error (isEOFError)


plus2 :: Int -> Int
plus2 = (+ 2)

guardEof
  :: (MonadUnliftIO m)
  => (Handle -> m a) -> Handle -> m (Maybe a)
guardEof hFunc h = eitherToMaybe <$> tryJust (guard . isEOFError) (hFunc h)


-- nice helper that is missing from data list extra in some versions
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

hReadUntilNotReady
  :: (MonadIO m)
  => Handle -> m ByteString
hReadUntilNotReady h = do
  hReady h >>=
    (\case
        True -> B.hGetSome h 64 >>= (\chars -> B.append chars <$> hReadUntilNotReady h)
        False -> return ""
    )

hPutStrLnWithTimeout
  :: (MonadUnliftIO m)
  => Int -- ^ maximum time in microseconds for line to be written
  -> Handle -- ^ output handle
  -> ByteString -- ^ what to write to the handle
  -> m Bool  -- ^ True = success, false = timed out
hPutStrLnWithTimeout maxTime h arg =
  timeout maxTime
    ( liftIO (BC.hPutStrLn h arg)
      >> hFlush h
    )
  <&> isJust


sleep :: MonadIO m => Int -> m ()
sleep = threadDelay . (* 1000000)

