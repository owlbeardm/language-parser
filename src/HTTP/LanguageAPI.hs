{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.LanguageAPI
      (
      LangsApi,
      languageServer
      ) where

import           ClassyPrelude          (map, return, ($))
import           Control.Monad.IO.Class (liftIO)
import           Database.Base          (runSQLAction)
import           Database.Entity        (Language)
import           Database.Esqueleto     (entityVal)
import           Database.Language      (listLangs)
import           Servant.API
import           Servant.Server

type LangsApi = "langs" :>
  (   Get '[JSON] [Language]
--   :<|> "all" :> Get '[JSON] [Language]
  )

languageServer :: Server LangsApi
languageServer = fetchLanguagesHandler
--   :<|> fetchLanguagesHandler

fetchLanguagesHandler ::  Handler [Language]
fetchLanguagesHandler = do
  langs <- liftIO $ runSQLAction listLangs
  return (map entityVal langs)


