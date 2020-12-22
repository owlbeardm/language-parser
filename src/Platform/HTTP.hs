{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Platform.HTTP
      (
      runServer,
      myApp
      ) where

import           ClassyPrelude            (IO, Text, map, return, ($))
import           Control.Monad.IO.Class   (liftIO)
-- import           Control.Monad.Trans.Except (throwE)
-- import           Data.Int                   (Int64)
import           Data.Proxy               (Proxy (..))
import           Database.Base            (runSQLAction)
import           Database.Entity
import           Database.Esqueleto       (entityVal)
import           Database.Language        (listLangs)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
-- import           Servant.Client
import           Servant.Server

type LanguageAPI =
       "api" :> "langs" :> Get '[JSON] [Language]
  :<|> "api" :> "hello" :> Get '[PlainText] Text

languageAPI :: Proxy LanguageAPI
languageAPI = Proxy :: Proxy LanguageAPI

fetchUsersHandler ::  Handler [Language]
fetchUsersHandler = do
  langs <- liftIO $ runSQLAction listLangs
  return (map entityVal langs)
--   case maybeUser of
--     Just user -> return user
--     Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

languageServer :: Server LanguageAPI
languageServer = fetchUsersHandler
 :<|> return "hello"

myApp :: Application
myApp = serve languageAPI languageServer

runServer :: IO ()
runServer = run 8000 myApp
--   connString <- fetchPostgresConnection

