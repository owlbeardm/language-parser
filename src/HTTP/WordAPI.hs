{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.WordAPI
      (
      WordsApi,
      wordServer
      ) where

import           ClassyPrelude          (Text, map, return, ($))
import           Control.Monad.IO.Class (liftIO)
import           Database.Base          (LanguageName (..), runSQLAction)
import           Database.Entity        (Language, Word)
import           Database.Esqueleto     (entityVal)
import           Database.Translation   (WordDescription, WordSource,
                                         WordTranslation,
                                         getFullWordDescription)
import           Database.Word          (findWordsByText, listWordsByLang)
import           Servant.API
import           Servant.Server

type WordDescriptionAPI = (Word, [Language], [WordTranslation], [WordSource])

type WordsApi = "words" :>
  (    Get '[JSON] [WordDescriptionAPI]
  :<|> Capture "word" Text :> Get '[JSON] [WordDescriptionAPI]
  )

wordServer :: Server WordsApi
wordServer =
       fetchWordsHandler
  :<|> lookUpWordsHandler

fetchWordsHandler ::  Handler [WordDescriptionAPI]
fetchWordsHandler = do
  words <- liftIO $ runSQLAction $ listWordsByLang langName
  fullDescr <- liftIO $ runSQLAction $ getFullWordDescription words
  return (map toWordDescriptionAPI fullDescr)
  where
    langName = Queran

lookUpWordsHandler :: Text -> Handler [WordDescriptionAPI]
lookUpWordsHandler word = do
  words <- liftIO $ runSQLAction $ findWordsByText word
  fullDescr <- liftIO $ runSQLAction $ getFullWordDescription words
  return (map toWordDescriptionAPI fullDescr)


toWordDescriptionAPI :: WordDescription -> WordDescriptionAPI
toWordDescriptionAPI (a, b, c, d) = (entityVal a, b, c, d)
