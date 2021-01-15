{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}


module HTTP.TranslationAPI
      (
      TranslationAPI,
      translationServer
      ) where

import           ClassyPrelude          (Int64, ($))
import           Control.Monad.IO.Class (liftIO)
import           Database.Base          (runSQLAction)
import           Database.Translation   (WordTranslation,
                                         getWordTranslationsByKey)
import           Servant.API
import           Servant.Server

type TranslationAPI = "translation" :>
  (    "bywordkey" :> Capture "wordId" Int64 :> Get '[JSON] [WordTranslation]
  )

translationServer :: Server TranslationAPI
translationServer = fetchTranslationsByKeyHandler

fetchTranslationsByKeyHandler :: Int64 -> Handler [WordTranslation]
fetchTranslationsByKeyHandler key = liftIO $ runSQLAction $ getWordTranslationsByKey key
