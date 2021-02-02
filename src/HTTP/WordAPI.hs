{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module HTTP.WordAPI
  ( WordsApi
  , wordServer
  ) where

import           ClassyPrelude        (Bool (..), Bounded, Enum, Eq, Generic,
                                       Int64, Maybe (..), MonadIO, Read, Show,
                                       Text, map, maxBound, minBound, return,
                                       ($), (.))
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Swagger         (ToParamSchema (..), ToSchema (..))
import           Database.Base        (LanguageName (..), PartOfSpeech (..),
                                       runDb)
import           Database.Esqueleto   (entityKey, entityVal, fromSqlKey)
import           Database.Language    (newWordWithOrigin)
import           Database.Translation (getFullWordDescription)
import           Database.Word        (addWordByLangNameF, deleteWordById,
                                       findWordsByText, findWordsByTextAndLang,
                                       getByWordByLangName, listWordsByLang,
                                       listWordsInfo, updateWordById)
import           HTTP.Config
import           HTTP.Utility         (AddWordJSON (..), WordDescriptionAPI,
                                       WordJSON (..), checkadded,
                                       convertWordDescriptionAPI,
                                       convertWordToWordJson)
import           Language.Word        (getWordsConstClusters,
                                       getWordsConstLastClusters,
                                       getWordsConstStartingClusters)
import           Servant.API
import           Servant.Server
import           Web.HttpApiData      (FromHttpApiData, parseQueryParam,
                                       readTextData)

type WordsApi
   = "words" :> ("lang" :> Capture "lang" LanguageName :> Get '[ JSON] [WordJSON] -- --
                  :<|> ReqBody '[ JSON] AddWordJSON :> Post '[ JSON] Bool -- --
                  :<|> Capture "wordId" Int64 :> Delete '[ JSON] () --
                  :<|> Capture "wordId" Int64 :> ReqBody '[ JSON] AddWordJSON :> Post '[ JSON] () --
                  :<|> "pos" :> Get '[ JSON] [PartOfSpeech] --
                  :<|> Capture "word" Text :> QueryParam "lang" LanguageName :> Get '[ JSON] [WordDescriptionAPI] --
                  :<|> "exists" :> ReqBody '[ JSON] AddWordJSON :> Post '[ JSON] Bool --
                  :<|> "constclusters" :> Capture "lang" LanguageName :> QueryParam "clusterType" ClusterType :> Get '[ JSON] [Text] --
                 ) --

data ClusterType
  = AllClusters
  | StartingCluster
  | LastClusters
  deriving ( Eq
           , Bounded
           , Enum
           , Read
           , Show
           , Generic
           , ToJSON
           , FromJSON
           , ToSchema
           , ToParamSchema
           )

instance FromHttpApiData ClusterType where
  parseQueryParam = readTextData

wordServer :: MonadIO m => ServerT WordsApi (AppServerT m)
wordServer =
  fetchWordsHandler :<|>  --
  addWord :<|>  --
  deleteWord :<|>  --
  updateWord :<|> --
  fetchPosHandler :<|> --
  lookUpWordsHandler :<|> --
  lookUpWordExistsHandler :<|> --
  fetchConstClusters --

fetchWordsHandler :: MonadIO m => LanguageName -> AppServerT m [WordJSON]
fetchWordsHandler langName = do
  words <- runDb $ listWordsByLang langName
  return (map makeWordJson words)
  where
    makeWordJson eWord = convertWordToWordJson (toInt eWord, entityVal eWord)
    toInt = fromSqlKey . entityKey

addWord :: MonadIO m => AddWordJSON -> AppServerT m Bool
addWord (AddWordJSON l p w f Nothing _) = do
  result <- runDb $ addWordByLangNameF w p l f
  return (checkadded result)
addWord (AddWordJSON l p w f (Just ot) oids) = do
  result <- runDb $ newWordWithOrigin w p l f ot oids
  return (checkadded result)

deleteWord :: MonadIO m => Int64 -> AppServerT m ()
deleteWord wordId = runDb $ deleteWordById wordId

updateWord :: MonadIO m => Int64 -> AddWordJSON -> AppServerT m ()
updateWord wordId (AddWordJSON _ p w f _ _) = runDb $ updateWordById wordId w p f

fetchPosHandler :: MonadIO m => AppServerT m [PartOfSpeech]
fetchPosHandler = return [minBound .. maxBound]

lookUpWordsHandler ::
     MonadIO m
  => Text
  -> Maybe LanguageName
  -> AppServerT m [WordDescriptionAPI]
lookUpWordsHandler wd mLang = do
  words <- runDb $ findWords wd mLang
  fullDescr <- runDb $ getFullWordDescription words
  return (map convertWordDescriptionAPI fullDescr)
  where
    findWords wrd Nothing         = findWordsByText wrd
    findWords wrd (Just langName) = findWordsByTextAndLang wrd langName

lookUpWordExistsHandler :: MonadIO m => AddWordJSON -> AppServerT m Bool
lookUpWordExistsHandler (AddWordJSON l p w _ _ _) = do
  mw <- runDb $ getByWordByLangName w l p
  case mw of
    Nothing -> return False
    _       -> return True

fetchConstClusters ::
     MonadIO m => LanguageName -> Maybe ClusterType -> AppServerT m [Text]
fetchConstClusters langName mClstType = do
  clstr <- runDb $ listWordsInfo langName (getClusterByType mClstType)
  return clstr
  where
    getClusterByType Nothing                = getWordsConstClusters
    getClusterByType (Just AllClusters)     = getWordsConstClusters
    getClusterByType (Just StartingCluster) = getWordsConstStartingClusters
    getClusterByType (Just LastClusters)    = getWordsConstLastClusters
