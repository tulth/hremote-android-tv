{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Shell (shell) where

import RIO
import qualified RIO.Process as RP
import qualified System.Process.Typed as P
import qualified Data.Text as T

shell
  :: (RP.HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => String -- ^ command to run
  -> (P.ProcessConfig () () () -> m a)
  -> m a
shell cmd inner = do
  wd <- view RP.workingDirL
  envStrings <- view RP.envVarsStringsL

  withProcessTimeLog wd cmd
    $ inner
    $ P.setEnv envStrings
    $ maybe id P.setWorkingDir wd
    $ P.shell cmd

withProcessTimeLog
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Maybe FilePath -- ^ working dir
  -> String -- ^ cmd
  -> m a
  -> m a
withProcessTimeLog mdir cmd proc' = do
  let cmdText = fromString cmd :: Text
      dirMsg =
        case mdir of
          Nothing -> ""
          Just dir -> " within " <> T.pack dir
  logDebug ("Run process" <> display dirMsg <> ": " <> display cmdText)
  start <- getMonotonicTime
  x <- proc'
  end <- getMonotonicTime
  let diff = end - start
  useColor <- view logFuncUseColorL
  accentColors <- view logFuncAccentColorsL
  logDebug
      ("Process finished in " <>
      (if useColor then accentColors 0 else "") <> -- accent color 0
      timeSpecMilliSecondText diff <>
      (if useColor then "\ESC[0m" else "") <> -- reset
       ": " <> display cmdText)
  return x


timeSpecMilliSecondText :: Double -> Utf8Builder
timeSpecMilliSecondText d = display (round (d * 1000) :: Int) <> "ms"
