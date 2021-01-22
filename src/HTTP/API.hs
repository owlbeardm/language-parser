{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.API
      (
      API,
      api,
      myApp,
      runServer
      ) where

import           ClassyPrelude            (IO, MonadIO, Text, return,
                                           runReaderT, ($))
import           Data.Proxy               (Proxy (..))
import           HTTP.Config
import           HTTP.LanguageAPI         (LangsApi, languageServer)
import           HTTP.TranslationAPI      (TranslationAPI, translationServer)
import           HTTP.WordAPI             (WordsApi, wordServer)
import           Network.Wai.Handler.Warp (run)
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

apiServer :: MonadIO m => ServerT API (AppServerT m)
apiServer = return "hello"
  :<|> languageServer
  :<|> wordServer
  :<|> translationServer

myApp :: Config -> Application
myApp cfg = serve api (appToServer cfg)

runServer :: Config -> IO ()
runServer cfg = run (configPort cfg) (myApp cfg)

convertApp :: Config -> AppServerT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

appToServer :: Config -> Server API
appToServer cfg = hoistServer api (convertApp cfg) apiServer
