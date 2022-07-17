{-# LANGUAGE LambdaCase #-}

module HandleHelpers
  (
    guardEof
  , hReadUntilNotReady
  , hPutStrLnWithTimeout
  ) where


import Data.Either.Extra (eitherToMaybe)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Control.Exception (tryJust)
import Control.Monad ( guard )
import System.IO (hGetChar
                 , hReady
                 , hPutStrLn
                 , hFlush
                 , Handle
                 )

import System.IO.Error (isEOFError)

import Timeout (timeout)

guardEof :: (Handle -> IO a) -> Handle -> IO (Maybe a)
guardEof hFunc h = eitherToMaybe <$> tryJust (guard . isEOFError) (hFunc h)

hReadUntilNotReady :: Handle -> IO String
hReadUntilNotReady h = do
  hReady h >>=
    (\case
        True -> hGetChar h >>= (\char -> (char :) <$> hReadUntilNotReady h)
        False -> return ""
    )

hPutStrLnWithTimeout :: Int -- ^ maximum time in microseconds for line to be written
  -> Handle -- ^ output handle
  -> String -- ^ what to write to the handle
  -> IO Bool  -- ^ True = success, false = timed out
hPutStrLnWithTimeout maxTime handle arg =
  timeout maxTime
    ( hPutStrLn handle arg
      >> hFlush handle
    )
  <&> isJust
