{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.API
      (
      runServer,
      myApp
      ) where

import           ClassyPrelude            (IO, Text, return)
import           Data.Proxy               (Proxy (..))
import           HTTP.LanguageAPI         (LangsApi, languageServer)
import           HTTP.WordAPI             (WordsApi, wordServer)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Server


type API = "api" :>
  (    "hello" :> Get '[PlainText] Text
  :<|> LangsApi
  :<|> WordsApi
  )

api :: Proxy API
api = Proxy :: Proxy API

apiServer :: Server API
apiServer = return "hello"
  :<|> languageServer
  :<|> wordServer

myApp :: Application
myApp = serve api apiServer

runServer :: IO ()
runServer = run 8000 myApp

