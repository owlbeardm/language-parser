{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.WordAPI
      (
      WordsApi,
      wordServer
      ) where

import           ClassyPrelude          (Bool (..), Maybe (..), Text, map,
                                         maxBound, minBound, return, ($), (.))
import           Control.Monad.IO.Class (liftIO)
import           Database.Base          (LanguageName (..), PartOfSpeech (..),
                                         runSQLAction)
import           Database.Esqueleto     (entityKey, entityVal, fromSqlKey)
import           Database.Translation   (WordDescription,
                                         getFullWordDescription)
import           Database.Word          (addWordByLangNameF, findWordsByText,
                                         findWordsByTextAndLang,
                                         getByWordByLangName, listWordsByLang)
import           HTTP.Utility           (AddWordJSON (..), WordDescriptionAPI,
                                         WordJSON (..), checkadded,
                                         convertWordDescriptionAPI,
                                         convertWordToWordJson)
import           Servant.API
import           Servant.Server

type WordsApi = "words" :>
  (    "lang" :> Capture "lang" LanguageName :> Get '[JSON] [WordJSON]
  :<|> ReqBody '[JSON] AddWordJSON :> Post '[JSON] Bool
  :<|> "pos" :> Get '[JSON] [PartOfSpeech]
  :<|> Capture "word" Text :> QueryParam "lang" LanguageName :> Get '[JSON] [WordDescriptionAPI]
  :<|> "exists" :> ReqBody '[JSON] AddWordJSON :>  Post '[JSON] Bool
  )

wordServer :: Server WordsApi
wordServer =
       fetchWordsHandler
  :<|> addWord
  :<|> fetchPosHandler
  :<|> lookUpWordsHandler
  :<|> lookUpWordExistsHandler

fetchWordsHandler ::  LanguageName -> Handler [WordJSON]
fetchWordsHandler langName = do
  words <- liftIO $ runSQLAction $ listWordsByLang langName
  return (map makeWordJson words)
  where
    makeWordJson eWord = convertWordToWordJson (toInt eWord, entityVal eWord)
    toInt = fromSqlKey . entityKey

addWord :: AddWordJSON -> Handler Bool
addWord (AddWordJSON l p w f) = do
  result <- liftIO $ runSQLAction $ addWordByLangNameF w p l f
  return (checkadded result)

fetchPosHandler :: Handler [PartOfSpeech]
fetchPosHandler = return [minBound..maxBound]

lookUpWordsHandler :: Text -> Maybe LanguageName -> Handler [WordDescriptionAPI]
lookUpWordsHandler word mLang = do
  words <- liftIO $ runSQLAction $ findWords word mLang
  fullDescr <- liftIO $ runSQLAction $ getFullWordDescription words
  return (map convertWordDescriptionAPI fullDescr)
  where
    findWords wrd Nothing         = findWordsByText wrd
    findWords wrd (Just langName) = findWordsByTextAndLang wrd langName

lookUpWordExistsHandler :: AddWordJSON -> Handler Bool
lookUpWordExistsHandler (AddWordJSON l p w _) = do
  mw <- liftIO $ runSQLAction $ getByWordByLangName w l p
  case mw of
    Nothing -> return False
    _       -> return True
