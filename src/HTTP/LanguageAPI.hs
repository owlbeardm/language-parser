{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.LanguageAPI
  ( LangsApi
  , languageServer
  ) where

import           ClassyPrelude      (MonadIO, map, return, ($), (.))
import           Database.Base      (LanguageName (..), runDb)
import           Database.Entity    (WordText, languageLname)
import           Database.Esqueleto (entityKey, entityVal, fromSqlKey)
import           Database.Language  (evolveLangsTo, getWordsToEvolve, listLangs,
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
                  :<|> "wordstoevolve" :> Capture "from" LanguageName :> Capture "to" LanguageName :> Get '[ JSON] [WordToEvolveJSON] --
                 )

languageServer :: MonadIO m => ServerT LangsApi (AppServerT m)
languageServer =
  fetchLanguagesHandler :<|> --
  traceWordHandler :<|> --
  fetchEvolveFromTo :<|> --
  fetchWordsToEvolve

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
    makeWordToEvolveJson (ew, wt) = WordToEvolveJSON (makeWordJson ew) wt
