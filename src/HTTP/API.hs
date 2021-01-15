{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.API
      (
      API,
      api,
      myApp,
      runServer
      ) where

import           ClassyPrelude            (IO, Text, return)
import           Data.Proxy               (Proxy (..))
import           HTTP.LanguageAPI         (LangsApi, languageServer)
import           HTTP.TranslationAPI      (TranslationAPI, translationServer)
import           HTTP.WordAPI             (WordsApi, wordServer)
import           Network.Wai.Handler.Warp (run)
-- import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant.API
import           Servant.Server



type API = "api" :>
  (    "hello" :> Get '[PlainText] Text
  :<|> LangsApi
  :<|> WordsApi
  :<|> TranslationAPI
  )

api :: Proxy API
api = Proxy :: Proxy API

apiServer :: Server API
apiServer = return "hello"
  :<|> languageServer
  :<|> wordServer
  :<|> translationServer

myApp :: Application
myApp = serve api apiServer

runServer :: IO ()
runServer = run 8000 myApp

