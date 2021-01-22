{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module HTTP.TranslationAPI
      (
      TranslationAPI,
      translationServer
      ) where

import           ClassyPrelude          (Bool (..), Int64, Maybe (..), MonadIO,
                                         map, return, throwIO, ($))
import           Database.Base          (runDb)
import           Database.Translation   (addAltTranslationFromId,
                                         addTranslationFromIdTo,
                                         getWordTranslationsByKey)
import           HTTP.Config
import           HTTP.Utility           (AddWordJSON (..),
                                         AddWordTranslationJSON (..),
                                         WordTranslationAPI, checkadded,
                                         convertWordTranslationAPI)
import           Servant.API
import           Servant.Server

type TranslationAPI = "translation" :>
  (    "bywordkey" :> Capture "wordId" Int64 :> Get '[JSON] [WordTranslationAPI]
  :<|> ReqBody '[JSON] AddWordTranslationJSON :> Post '[JSON] Bool
  )

translationServer :: MonadIO m => ServerT TranslationAPI (AppServerT m)
translationServer = fetchTranslationsByKeyHandler
  :<|> addTranslation

fetchTranslationsByKeyHandler :: MonadIO m => Int64 -> AppServerT m [WordTranslationAPI]
fetchTranslationsByKeyHandler key = do
  wt <- runDb $ getWordTranslationsByKey key
  return (map convertWordTranslationAPI wt)

addTranslation :: MonadIO m => AddWordTranslationJSON -> AppServerT m Bool
addTranslation awt = do
  key <- if isAltTranslation awt
    then
      case altTranslation awt of
        Nothing -> throwIO $ err400 { errBody = "Your request makes no sense to me." }
        Just atr -> runDb $ addAltTranslationFromId (wordFromId awt) (langTo awt) atr (comment awt)
    else
      case translation awt of
        Nothing -> throwIO $ err400 { errBody = "Your request makes no sense to me." }
        Just tr -> runDb $ addTranslationFromIdTo (wordFromId awt) (wordText tr) (pos tr) (langTo awt) (comment awt)
  return (checkadded key)
