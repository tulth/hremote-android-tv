{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import RIO
import Types
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_hremote_android_tv

import ServerApp (startServer)


main :: IO ()
-- main = startApp
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_hremote_android_tv.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> strOption ( long "mqttbroker"
                       <> short 'm'
                       <> value defMqttBroker
                       <> help ( "mqtt broker host name (default: " <> show defMqttBroker <> ")")
                     )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
  where defMqttBroker = "mqtt-broker"


run :: RIO App ()
run = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  queue <- newTBQueueIO 8
  logInfo "Starting up"
  forever $ do
    race_
      -- (mqttLoop queue)
      (liftIO $ startServer queue)
      (eventSendLoop queue)
    logError "mainApp: eventProcess or mqttWatch died, restarting"

