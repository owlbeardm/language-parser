{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.WordAPI
      (
      WordsApi,
      wordServer
      ) where

import           ClassyPrelude          (Int64, Maybe (..), Text, map, return,
                                         show, unpack, ($), (.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (ToJSON, object, toJSON, (.=))
import           Database.Base          (LanguageName (..), runSQLAction)
import           Database.Entity        (Language, Word, wordForgotten,
                                         wordPartOfSpeech, wordWord)
import           Database.Esqueleto     (entityKey, entityVal, fromSqlKey)
import           Database.Translation   (WordDescription, WordSource,
                                         WordTranslation,
                                         getFullWordDescription)
import           Database.Word          (findWordsByText,
                                         findWordsByTextAndLang,
                                         listWordsByLang)
import           Servant.API
import           Servant.Server

type WordDescriptionAPI = (Word, [Language], [WordTranslation], [WordSource])
data WordJSON = WordJSON Int64 Word

instance ToJSON WordJSON where
  toJSON (WordJSON key word) = object
    [ "id" .= key,
      "word" .= unpack (wordWord word),
      "partOfSpeech" .= show (wordPartOfSpeech word),
      "forgotten" .= wordForgotten word
    ]

type WordsApi = "words" :>
  (    "lang" :> Capture "lang" LanguageName :> Get '[JSON] [WordJSON]
  :<|> Capture "word" Text :> QueryParam "lang" LanguageName :> Get '[JSON] [WordDescriptionAPI]
  )

wordServer :: Server WordsApi
wordServer =
       fetchWordsHandler
  :<|> lookUpWordsHandler

fetchWordsHandler ::  LanguageName -> Handler [WordJSON]
fetchWordsHandler langName = do
  words <- liftIO $ runSQLAction $ listWordsByLang langName
  return (map makeWordJson words)
  where
    makeWordJson eWord = WordJSON (toInt eWord) (entityVal eWord)
    toInt = fromSqlKey . entityKey
--   fullDescr <- liftIO $ runSQLAction $ getFullWordDescription words
--   return (map toWordDescriptionAPI fullDescr)
--   where
--     -- listWrds :: (MonadIO m) => Maybe LanguageName -> AppT m [Entity Word]
--     listWrds Nothing = []
--     listWrds (Just langName) = listWordsByLang langName

lookUpWordsHandler :: Text -> Maybe LanguageName -> Handler [WordDescriptionAPI]
lookUpWordsHandler word mLang = do
  words <- liftIO $ runSQLAction $ findWords word mLang
  fullDescr <- liftIO $ runSQLAction $ getFullWordDescription words
  return (map toWordDescriptionAPI fullDescr)
  where
    findWords wrd Nothing         = findWordsByText wrd
    findWords wrd (Just langName) = findWordsByTextAndLang wrd langName


toWordDescriptionAPI :: WordDescription -> WordDescriptionAPI
toWordDescriptionAPI (a, b, c, d) = (entityVal a, b, c, d)
