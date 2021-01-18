{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module HTTP.TranslationAPI
      (
      TranslationAPI,
      translationServer
      ) where

import           ClassyPrelude          (Bool (..), Int64, Maybe (..), map,
                                         return, throwIO, ($))
import           Control.Monad.IO.Class (liftIO)
import           Database.Base          (runSQLAction)
import           Database.Translation   (addAltTranslationFromId,
                                         addTranslationFromIdTo,
                                         getWordTranslationsByKey)
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

translationServer :: Server TranslationAPI
translationServer = fetchTranslationsByKeyHandler
  :<|> addTranslation

fetchTranslationsByKeyHandler :: Int64 -> Handler [WordTranslationAPI]
fetchTranslationsByKeyHandler key = do
  wt <- liftIO $ runSQLAction $ getWordTranslationsByKey key
  return (map convertWordTranslationAPI wt)

addTranslation :: AddWordTranslationJSON -> Handler Bool
addTranslation awt = do
  key <- liftIO $ if isAltTranslation awt
    then
      case altTranslation awt of
        Nothing -> throwIO $ err400 { errBody = "Your request makes no sense to me." }
        Just atr -> runSQLAction $ addAltTranslationFromId (wordFromId awt) (langTo awt) atr (comment awt)
    else
      case translation awt of
        Nothing -> throwIO $ err400 { errBody = "Your request makes no sense to me." }
        Just tr -> runSQLAction $ addTranslationFromIdTo (wordFromId awt) (wordText tr) (pos tr) (langTo awt) (comment awt)
  return (checkadded key)
