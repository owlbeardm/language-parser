{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Platform.HTTP
      (
      runServer
      ) where

import           ClassyPrelude            (IO, map, return, ($))
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
--   :<|> "api" :> "langs" :> ReqBody '[JSON] Language :> Post '[JSON] Int64

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
--  :<|> (createUserHandler connString)

runServer :: IO ()
runServer = run 8000 (serve languageAPI languageServer)
--   connString <- fetchPostgresConnection

