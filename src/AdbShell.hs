{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module AdbShell
  (
    withAdbShell
  , adbShellCatch
  ) where

import RIO
import RIO.Process

import Shell

import GHC.IO.Exception (ioe_type, IOErrorType(ResourceVanished, NoSuchThing))

mkAdbConnectCommand :: String -> String
mkAdbConnectCommand adbTarget = "adb connect " ++ adbTarget

mkAdbShellCommand :: String -> String
mkAdbShellCommand adbTarget = "adb -s " ++ adbTarget ++ " shell"

adbShellCatch
  :: (MonadUnliftIO m)
  => m () -- ^ action to run pre-catch
  -> m () -- ^ action to run on resource vanished
  -> m () -- ^ action to run on no such thing
  -> m () -- ^ action to run on exit code exception
  -> m ()
adbShellCatch action resourceVanishedHandler noSuchThingHandler exitCodeExceptionHandler =
  catch action
    (\e ->
      if ioe_type e == ResourceVanished then resourceVanishedHandler
      else if ioe_type e == NoSuchThing then noSuchThingHandler
      else throwIO e)
    `catch` (\case ExitCodeException {} -> exitCodeExceptionHandler)

withAdbShell
  :: (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  => String -- ^ adb target host name
  -> (Process Handle Handle () -> m ()) -- ^ given the process, perform actions
  -> m ()
withAdbShell adbHost adbClient = do
  logInfo $ fromString $ "connecting to adb with command: " ++ adbConnectCmd
  shell adbConnectCmd $ \processConfig ->
    let customProcessConfig =
          setCreateGroup True $
          setNewSession True $
          setStdin createPipe $
          setStdout createPipe $
          setStderr closed processConfig
    in withProcessWait customProcessConfig (const $ return ())
  logInfo $ fromString $ "launching adb shell with command: " ++ adbShellCmd
  shell adbShellCmd $ \processConfig ->
    let customProcessConfig =
          setCreateGroup True $
          setNewSession True $
          setStdin createPipe $
          setStdout createPipe $
          setStderr closed processConfig
    in withProcessWait customProcessConfig adbClient
  where adbConnectCmd = mkAdbConnectCommand adbHost
        adbShellCmd = mkAdbShellCommand adbHost
