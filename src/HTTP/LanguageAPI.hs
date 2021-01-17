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
import           Database.Entity        (Language, WordText)
import           Database.Esqueleto     (entityVal)
import           Database.Language      (listLangs, traceWordEvolve)
import           HTTP.Utility           (TraceWordReq (..))
import           Servant.API
import           Servant.Server

type LangsApi = "langs" :>
  (    Get '[JSON] [Language]
  :<|> "traceWord" :> ReqBody '[JSON] TraceWordReq :> Post '[JSON] [WordText]
  )

languageServer :: Server LangsApi
languageServer = fetchLanguagesHandler
  :<|> traceWordHandler

fetchLanguagesHandler :: Handler [Language]
fetchLanguagesHandler = do
  l <- liftIO $ runSQLAction listLangs
  return (map entityVal l)

traceWordHandler :: TraceWordReq -> Handler [WordText]
traceWordHandler twr = liftIO $ runSQLAction $ traceWordEvolve (wordTrace twr) (langs twr)

