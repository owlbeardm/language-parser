{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.WordAPI
      (
      WordsApi,
      wordServer
      ) where

import           ClassyPrelude          (Bool (..), Int64, Maybe (..), MonadIO,
                                         Text, map, maxBound, minBound, return,
                                         ($), (.))
import           Database.Base          (LanguageName (..), PartOfSpeech (..),
                                         runDb)
import           Database.Esqueleto     (entityKey, entityVal, fromSqlKey)
import           Database.Translation   (getFullWordDescription)
import           Database.Word          (addWordByLangNameF, deleteWordById,
                                         findWordsByText,
                                         findWordsByTextAndLang,
                                         getByWordByLangName, listWordsByLang,
                                         updateWordById)
import           HTTP.Config
import           HTTP.Utility           (AddWordJSON (..), WordDescriptionAPI,
                                         WordJSON (..), checkadded,
                                         convertWordDescriptionAPI,
                                         convertWordToWordJson)
import           Servant.API
import           Servant.Server

type WordsApi = "words" :>
  (    "lang" :> Capture "lang" LanguageName :> Get '[JSON] [WordJSON]
  :<|> ReqBody '[JSON] AddWordJSON :> Post '[JSON] Bool
  :<|> Capture "wordId" Int64 :> Delete '[JSON] ()
  :<|> Capture "wordId" Int64 :> ReqBody '[JSON] AddWordJSON :> Post '[JSON] ()
  :<|> "pos" :> Get '[JSON] [PartOfSpeech]
  :<|> Capture "word" Text :> QueryParam "lang" LanguageName :> Get '[JSON] [WordDescriptionAPI]
  :<|> "exists" :> ReqBody '[JSON] AddWordJSON :>  Post '[JSON] Bool
  )

wordServer :: MonadIO m => ServerT WordsApi (AppServerT m)
wordServer =
       fetchWordsHandler
  :<|> addWord
  :<|> deleteWord
  :<|> updateWord
  :<|> fetchPosHandler
  :<|> lookUpWordsHandler
  :<|> lookUpWordExistsHandler

fetchWordsHandler :: MonadIO m => LanguageName -> AppServerT m [WordJSON]
fetchWordsHandler langName = do
  words <- runDb $ listWordsByLang langName
  return (map makeWordJson words)
  where
    makeWordJson eWord = convertWordToWordJson (toInt eWord, entityVal eWord)
    toInt = fromSqlKey . entityKey

addWord :: MonadIO m => AddWordJSON -> AppServerT m Bool
addWord (AddWordJSON l p w f) = do
  result <- runDb $ addWordByLangNameF w p l f
  return (checkadded result)

deleteWord :: MonadIO m => Int64 -> AppServerT m ()
deleteWord wordId = runDb $ deleteWordById wordId

updateWord :: MonadIO m => Int64 -> AddWordJSON -> AppServerT m ()
updateWord wordId (AddWordJSON _ p w f) = runDb $ updateWordById wordId w p f

fetchPosHandler :: MonadIO m => AppServerT m [PartOfSpeech]
fetchPosHandler = return [minBound..maxBound]

lookUpWordsHandler :: MonadIO m => Text -> Maybe LanguageName -> AppServerT m [WordDescriptionAPI]
lookUpWordsHandler wd mLang = do
  words <- runDb $ findWords wd mLang
  fullDescr <- runDb $ getFullWordDescription words
  return (map convertWordDescriptionAPI fullDescr)
  where
    findWords wrd Nothing         = findWordsByText wrd
    findWords wrd (Just langName) = findWordsByTextAndLang wrd langName

lookUpWordExistsHandler :: MonadIO m => AddWordJSON -> AppServerT m Bool
lookUpWordExistsHandler (AddWordJSON l p w _) = do
  mw <- runDb $ getByWordByLangName w l p
  case mw of
    Nothing -> return False
    _       -> return True
