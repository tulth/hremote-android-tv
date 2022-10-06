{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ServerApp
    ( startServer
    , serverApp
    ) where

import Servant
    ( serve
      , Application
      , type (:>)
      , PlainText
      , Get
      , Post
      , Proxy
      , Handler
      , ReqBody
      , Capture
      , type  (:<|>) ((:<|>))
      )
import Network.Wai ( Middleware )
import Network.Wai.Logger ( withStdoutLogger )
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Handler.Warp
    ( setLogger, setPort, runSettings, defaultSettings )
import Servant.Server ( Server )
import Data.Proxy (Proxy(Proxy))

import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import Data.List (intercalate)
import Control.Monad.IO.Class (MonadIO(liftIO))

import RIO (TBQueue, writeTBQueue, atomically)

import Events (buttonToEventCode)

type API = "status" :> Get '[PlainText] String
           :<|> Get '[HTML] Remotepage
           :<|> "button" :> Capture "buttonName" ButtonName
                         :> ReqBody '[PlainText] ButtonAction
                         :> Post '[PlainText] ButtonAction

type ButtonName = String
type ButtonAction = String

type Remotepage = H.Html

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where policy = simpleCorsResourcePolicy
          {
            corsMethods = [ "GET", "POST", "PUT", "OPTIONS" ]
          -- , corsOrigins = Just (["http://burpelson:8080"], False)
          , corsOrigins = Nothing
          , corsRequestHeaders = [ "Content-Type" ]
          }

startServer :: TBQueue Int -> IO ()
startServer queue = do
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8002 $ setLogger aplogger defaultSettings
        runSettings settings $ serverApp queue

serverApp :: TBQueue Int -> Application
serverApp queue = logStdoutDev $ corsPolicy $ serve api $ server queue

api :: Proxy API
api = Proxy

server :: TBQueue Int -> Server API
server queue = statusHandler
    :<|> remoteHandler
    :<|> buttonHandler queue
  where statusHandler :: Handler String
        statusHandler = return "up"
        remoteHandler = return remoteDemo

buttonHandler :: TBQueue Int -> ButtonName -> ButtonAction -> Handler ButtonAction
buttonHandler queue button action = do
  liftIO $ print $ "got: " ++ button ++ " " ++ action ++ " " ++ show mEventCode
  case mEventCode of
    Just eventCode -> atomically $ writeTBQueue queue eventCode
    Nothing -> return ()
  return action
  where mEventCode = buttonToEventCode button

buttons :: [String]
buttons = ["back"
          , "dpad_up"
          , "home"
          , "dpad_left"
          , "dpad_center"
          , "dpad_right"
          , "volume_down"
          , "dpad_down"
          , "volume_up"]

buttonLabel :: String -> String
buttonLabel "dpad_up" = "up"
buttonLabel "dpad_down" = "down"
buttonLabel "dpad_left" = "left"
buttonLabel "dpad_center" = "center"
buttonLabel "dpad_right" = "right"
buttonLabel "volume_down" = "vol-"
buttonLabel "volume_up" = "vol+"
buttonLabel same = same

remoteDemo :: H.Html
remoteDemo =
 H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
      H.title "tv-remote"
      H.link ! A.rel "shortcut icon" ! A.type_ "image/png" ! A.href "http://nginx:8088/assets/icons/tv-remote.png"
      H.preEscapedText $ ""
        <> "<style>"
        <> "  * {"
        <> "      box-sizing: border-box"
        <> "  }"
        <> "  body {background-color: black;}"
        <> "  .grid-dirpad {"
        <> "      display: grid;"
        <> "      grid-gap: 2%;"
        <> "      grid-template-columns: repeat(3, 1fr);"
        <> "      height: 90vh;"
        <> "      background-color: black;"
        <> "      /* max-width: 500px; */"
        <> "  }"
        <> "  .button {"
        <> "      /* background-color: #337; */"
        <> "      /* color: #CCF; */"
        <> "      /* border: 2px solid #335; */"
        <> "      display: block;"
        <> "      margin: 5px 0;"
        <> "      padding: 5px;"
        <> "      width: 100%;"
        <> "      height: 100%;"
        <> "  }"
        <> "</style>"
    H.body $ do
      H.div ! A.class_ "grid-dirpad" $ do
        let mkGridButton button =
              H.div $ H.button ! A.class_ "button" !
                A.id (H.stringValue button) $ H.toHtml (buttonLabel button)
        mapM_ mkGridButton buttons
      H.script $ "\n"
        <> "var url_base = \"/button/\";\n"
        <> "var id_array = [\n"
        <> H.toHtml (intercalate ",\n    " $ map (\b -> "\"" <> b <> "\"") buttons)
        <> "];\n"
        <> "function nvKeyPress(item) {\n"
        <> "    var url = url_base + item;\n"
        <> "    var xhr = new XMLHttpRequest();\n"
        <> "    xhr.open(\"POST\", url);\n"
        <> "    xhr.setRequestHeader(\"Content-Type\", \"text/plain;charset=utf-8\");\n"
        <> "    xhr.onreadystatechange = function () {\n"
        <> "        if (xhr.readyState === 4) {\n"
        <> "            console.log(xhr.status);\n"
        <> "            console.log(xhr.responseText);\n"
        <> "        }};\n"
        <> "\n"
        <> "    var data = 'ON';\n"
        <> "\n"
        <> "    xhr.send(data);\n"
        <> "}\n"
        <> "id_array.forEach(function (item, index) {\n"
        <> "    const button = document.getElementById(item);\n"
        <> "    button.addEventListener('click', async _ => {\n"
        <> "        nvKeyPress(item);\n"
        <> "    });\n"
        <> "});\n"
        <> "document.addEventListener('keydown', function (event) {\n"
        <> "    if (event.key === 'ArrowUp') {\n"
        <> "        nvKeyPress('dpad_up');\n"
        <> "    }\n"
        <> "    if (event.key === 'ArrowDown') {\n"
        <> "        nvKeyPress('dpad_down');\n"
        <> "    }\n"
        <> "    if (event.key === 'ArrowLeft') {\n"
        <> "        nvKeyPress('dpad_left');\n"
        <> "    }\n"
        <> "    if (event.key === 'ArrowRight') {\n"
        <> "        nvKeyPress('dpad_right');\n"
        <> "    }\n"
        <> "    if ([\" \"\n"
        <> "         // , \"Enter\"\n"
        <> "        ]\n"
        <> "        .includes(event.key)) {\n"
        <> "        nvKeyPress('dpad_center');\n"
        <> "    }\n"
        <> "    if ([\"H\", \"h\", \"Home\"].includes(event.key)) {\n"
        <> "        nvKeyPress('home');\n"
        <> "    }\n"
        <> "    if ([\"b\", \"B\", \"Escape\"].includes(event.key)) {\n"
        <> "        nvKeyPress('back');\n"
        <> "    }\n"
        <> "    if ([\"+\"].includes(event.key)) {\n"
        <> "        nvKeyPress('volume_up');\n"
        <> "    }\n"
        <> "    if ([\"-\"].includes(event.key)) {\n"
        <> "        nvKeyPress('volume_down');\n"
        <> "    }\n"
        <> "});\n"

