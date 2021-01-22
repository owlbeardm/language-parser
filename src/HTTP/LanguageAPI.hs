{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module HTTP.LanguageAPI
      (
      LangsApi,
      languageServer
      ) where

import           ClassyPrelude          (MonadIO, map, return, ($), (.))
import           Database.Base          (LanguageName (..), runDb)
import           Database.Entity        (WordText, languageLname)
import           Database.Esqueleto     (entityVal)
import           Database.Language      (listLangs, traceWordEvolve)
import           HTTP.Config
import           HTTP.Utility           (TraceWordReq (..))
import           Servant.API
import           Servant.Server

type LangsApi = "langs" :>
  (    Get '[JSON] [LanguageName]
  :<|> "traceWord" :> ReqBody '[JSON] TraceWordReq :> Post '[JSON] [WordText]
  )

languageServer :: MonadIO m => ServerT LangsApi (AppServerT m)
languageServer = fetchLanguagesHandler
  :<|> traceWordHandler

fetchLanguagesHandler :: MonadIO m => AppServerT m [LanguageName]
fetchLanguagesHandler = do
  l <- runDb listLangs
  return (map (languageLname . entityVal) l)

traceWordHandler :: MonadIO m => TraceWordReq -> AppServerT m [WordText]
traceWordHandler twr = runDb $ traceWordEvolve (wordTrace twr) (langs twr)

