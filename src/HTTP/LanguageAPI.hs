{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module HTTP.LanguageAPI
      (
      LangsApi,
      languageServer
      ) where

import           ClassyPrelude          (map, return, ($), (.))
import           Control.Monad.IO.Class (liftIO)
import           Database.Base          (LanguageName (..), runSQLAction)
import           Database.Entity        (Language, WordText, languageLname)
import           Database.Esqueleto     (entityVal)
import           Database.Language      (listLangs, traceWordEvolve)
import           HTTP.Utility           (TraceWordReq (..))
import           Servant.API
import           Servant.Server

type LangsApi = "langs" :>
  (    Get '[JSON] [LanguageName]
  :<|> "traceWord" :> ReqBody '[JSON] TraceWordReq :> Post '[JSON] [WordText]
  )

languageServer :: Server LangsApi
languageServer = fetchLanguagesHandler
  :<|> traceWordHandler

fetchLanguagesHandler :: Handler [LanguageName]
fetchLanguagesHandler = do
  l <- liftIO $ runSQLAction listLangs
  return (map (languageLname . entityVal) l)

traceWordHandler :: TraceWordReq -> Handler [WordText]
traceWordHandler twr = liftIO $ runSQLAction $ traceWordEvolve (wordTrace twr) (langs twr)

