{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.LanguageAPI
  ( LangsApi
  , languageServer
  ) where

import           ClassyPrelude      (Int, Maybe (..), MonadIO, map, return, ($),
                                     (.))
import           Database.Base      (LanguageName (..), runDb)
import           Database.Entity    (WordText, languageLname)
import           Database.Esqueleto (entityKey, entityVal, fromSqlKey)
import           Database.Language  (evolveLang, evolveLangsTo, getWordsEvolved,
                                     getWordsToEvolve, listLangs, reEvolveLang,
                                     traceWordEvolve)
import           HTTP.Config
import           HTTP.Utility       (TraceWordReq (..), WordToEvolveJSON (..),
                                     convertWordToWordJson)
import           Servant.API
import           Servant.Server

type LangsApi
   = "langs" :> (Get '[ JSON] [LanguageName] --
                  :<|> "traceWord" :> ReqBody '[ JSON] TraceWordReq :> Post '[ JSON] [WordText] --
                  :<|> "evolvefrom" :> Capture "lang" LanguageName :> Get '[ JSON] [LanguageName] --
                  :<|> "wordsevolved" :> Capture "from" LanguageName :> Capture "to" LanguageName :> Get '[ JSON] [WordToEvolveJSON] --
                  :<|> "wordstoevolve" :> Capture "from" LanguageName :> Capture "to" LanguageName :> Get '[ JSON] [WordToEvolveJSON] --
                  :<|> "evolve" :> Capture "from" LanguageName :> Capture "to" LanguageName :> Post '[ JSON] Int --
                  :<|> "reevolve" :> Capture "from" LanguageName :> Capture "to" LanguageName :> Post '[ JSON] Int --
                 )

languageServer :: MonadIO m => ServerT LangsApi (AppServerT m)
languageServer =
  fetchLanguagesHandler :<|> --
  traceWordHandler :<|> --
  fetchEvolveFromTo :<|> --
  fetchWordsEvolved :<|> --
  fetchWordsToEvolve :<|> --
  evolve :<|> --
  reevolve

fetchLanguagesHandler :: MonadIO m => AppServerT m [LanguageName]
fetchLanguagesHandler = do
  l <- runDb listLangs
  return (map (languageLname . entityVal) l)

traceWordHandler :: MonadIO m => TraceWordReq -> AppServerT m [WordText]
traceWordHandler twr = runDb $ traceWordEvolve (wordTrace twr) (langs twr)

fetchEvolveFromTo :: MonadIO m => LanguageName -> AppServerT m [LanguageName]
fetchEvolveFromTo evolveFrom = do
  l <- runDb $ evolveLangsTo evolveFrom
  return (map (languageLname . entityVal) l)

fetchWordsEvolved ::
     MonadIO m
  => LanguageName
  -> LanguageName
  -> AppServerT m [WordToEvolveJSON]
fetchWordsEvolved evolveFrom evolveTo = do
  wordsToEvolve <- runDb $ getWordsEvolved evolveFrom evolveTo
  return (map makeWordToEvolveJson wordsToEvolve)
  where
    toInt = fromSqlKey . entityKey
    makeWordJson eWord = convertWordToWordJson (toInt eWord, entityVal eWord)
    makeWordToEvolveJson (ew, wt, wrdse) = WordToEvolveJSON (makeWordJson ew) wt (Just (map makeWordJson wrdse))

fetchWordsToEvolve ::
     MonadIO m
  => LanguageName
  -> LanguageName
  -> AppServerT m [WordToEvolveJSON]
fetchWordsToEvolve evolveFrom evolveTo = do
  wordsToEvolve <- runDb $ getWordsToEvolve evolveFrom evolveTo
  return (map makeWordToEvolveJson wordsToEvolve)
  where
    toInt = fromSqlKey . entityKey
    makeWordJson eWord = convertWordToWordJson (toInt eWord, entityVal eWord)
    makeWordToEvolveJson (ew, wt) = WordToEvolveJSON (makeWordJson ew) wt Nothing

evolve :: MonadIO m => LanguageName -> LanguageName -> AppServerT m Int
evolve langName1 langName2 = do
  result <- runDb $ evolveLang langName1 langName2
  case result of
    Just (size,_,_) -> return size
    Nothing -> return 0

reevolve :: MonadIO m => LanguageName -> LanguageName -> AppServerT m Int
reevolve langName1 langName2 = do
  result <- runDb $ reEvolveLang langName1 langName2
  case result of
    Just (size,_,_) -> return size
    Nothing -> return 0
