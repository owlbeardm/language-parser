{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}


module HTTP.LanguageAPI
      (
      LangsApi,
      languageServer
      ) where

import           ClassyPrelude          (Generic, map, return, ($))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
-- import           Data.Aeson             (FromJSON, Object, ToJSON, parseJSON,
--                                          withObject, (.:))
-- import           Data.Aeson.Types       (Parser)
import           Data.Swagger           (ToSchema)
import           Database.Base          (LanguageName, runSQLAction)
import           Database.Entity        (Language, WordText)
import           Database.Esqueleto     (entityVal)
import           Database.Language      (listLangs, traceWordEvolve)
import           Servant.API
import           Servant.Server

type LangsApi = "langs" :>
  (    Get '[JSON] [Language]
  :<|> "traceWord" :> ReqBody '[JSON] TraceWordReq :> Post '[JSON] [WordText]
  )

languageServer :: Server LangsApi
languageServer = fetchLanguagesHandler
  :<|> traceWordHandler

data TraceWordReq = TraceWordReq { word  :: WordText
                                 , langs :: [LanguageName]
                                 }
                                 deriving (Generic, ToJSON, FromJSON, ToSchema)

-- instance FromJSON TraceWordReq where
--   parseJSON = withObject "TraceWordReq" parseTraceWordReq

-- parseTraceWordReq :: Object -> Parser TraceWordReq
-- parseTraceWordReq o = do
--   w <- o .: "word"
--   l <- o .: "langs"
--   return TraceWordReq {word = w, langs = l}

fetchLanguagesHandler :: Handler [Language]
fetchLanguagesHandler = do
  l <- liftIO $ runSQLAction listLangs
  return (map entityVal l)

traceWordHandler :: TraceWordReq -> Handler [WordText]
traceWordHandler twr = liftIO $ runSQLAction $ traceWordEvolve (word twr) (langs twr)

