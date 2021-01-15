{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.WordAPI
      (
      WordsApi,
      wordServer
      ) where

import           ClassyPrelude          (Maybe(..), Text, map,
                                         return, ($))
import           Control.Monad.IO.Class (liftIO)
import           Database.Base          (LanguageName (..), runSQLAction)
import           Database.Entity        (Language, Word)
import           Database.Esqueleto     (entityVal)
import           Database.Translation   (WordDescription, WordSource,
                                         WordTranslation,
                                         getFullWordDescription)
import           Database.Word          (findWordsByText,
                                         findWordsByTextAndLang,
                                         listWordsByLang)
import           Servant.API
import           Servant.Server

type WordDescriptionAPI = (Word, [Language], [WordTranslation], [WordSource])

type WordsApi = "words" :>
  (    QueryParam "lang" LanguageName :> Get '[JSON] [WordDescriptionAPI]
  :<|> Capture "word" Text :> QueryParam "lang" LanguageName :> Get '[JSON] [WordDescriptionAPI]
  )

wordServer :: Server WordsApi
wordServer =
       fetchWordsHandler
  :<|> lookUpWordsHandler

fetchWordsHandler ::  Maybe LanguageName -> Handler [WordDescriptionAPI]
fetchWordsHandler mLang = do
  words <- liftIO $ runSQLAction $ listWrds mLang
  fullDescr <- liftIO $ runSQLAction $ getFullWordDescription words
  return (map toWordDescriptionAPI fullDescr)
  where
    -- listWrds :: (MonadIO m) => Maybe LanguageName -> AppT m [Entity Word]
    listWrds Nothing = []
    listWrds (Just langName) = listWordsByLang langName

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
