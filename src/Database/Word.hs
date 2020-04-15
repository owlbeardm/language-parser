module Database.Word where

import           ClassyPrelude                hiding (Word, on, nothing)
-- import           Control.Monad.Logger         (LoggingT, NoLoggingT, logErrorN,
--                                                logErrorNS, runNoLoggingT,
--                                                runStderrLoggingT)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Persist.TH

addWord :: (MonadIO m, MonadLogger m) => Text -> PartOfSpeech -> LanguageName -> AppT m (Maybe (Key Word))
addWord word pos langName = do
  lang <- getBy $ LanguageNameUnq langName
  case lang of
    Nothing -> do
      logErrorNS "addWord" "There is no such lang in the database"
      return Nothing
    (Just l) -> insertUnique $ Word word (entityKey l) pos

listWordsByLang :: (MonadIO m) => LanguageName -> AppT m [Entity Word]
listWordsByLang langName = select $ from $ \(word,lang) -> do
      where_ (word ^. WordLangId ==. lang ^. LanguageId &&.
              lang ^. LanguageLname ==. val langName )
      return word

-- listNotEvolvedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity Word]
-- listNotEvolvedWordsByLangFromAndTo langNameFrom langNameTo =
--   select $
--   from $ \((word,langFrom) `LeftOuterJoin` (wordTo,langTo,wordOrg,wordOrgFrom)) -> do
--       on (word ^. WordId ==. wordOrgFrom ^. WordOriginFromWordFromId)
--       where_ (just (wordOrgFrom ^. WordOriginFromWordFromId) ==. nothing &&.
--               word ^. WordLangId ==. langFrom ^. LanguageId &&.
--               langFrom ^. LanguageLname ==. val langNameFrom &&.
--               wordTo ^. WordLangId ==. langTo ^. LanguageId &&.
--               langTo ^. LanguageLname ==. val langNameTo &&.
--               wordOrg ^. WordOriginWordId ==. wordTo ^. WordId &&.
--               wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
--       return word


listNotEvolvedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity Word]
listNotEvolvedWordsByLangFromAndTo langNameFrom langNameTo =
  select $
  from $ \((word `InnerJoin` langFrom) `LeftOuterJoin` (wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo)) -> do
      on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
      on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
      on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
      on (word ^. WordId ==. wordOrgFrom ^. WordOriginFromWordFromId)
      on (word ^. WordLangId ==. langFrom ^. LanguageId)
      where_ (just (wordOrgFrom ^. WordOriginFromWordFromId) ==. nothing &&.
              langFrom ^. LanguageLname ==. val langNameFrom &&.
              langTo ^. LanguageLname ==. val langNameTo)
      return word

getWord :: (MonadIO m, MonadLogger m) =>
     Text
  -> PartOfSpeech
  -> LanguageName
  -> LanguageName
  -> AppT m (Maybe (Key Word))
getWord wordText posWord fromLang toLang = do
    mLangFrom <- getLang fromLang
    mLangTo <- getLang toLang
    case (mLangFrom, mLangTo) of
      (Nothing, _) ->  do
        logErrorNS "addTranslationFromTo" "There is no such lang from in the database"
        return Nothing
      (_, Nothing) ->  do
        logErrorNS "addTranslationFromTo" "There is no such lang to in the database"
        return Nothing
      (Just langFrom, Just langTo) -> do
        mWord <-
          getBy $ WordWordPosLangIdUnq wordText posWord (entityKey langFrom)
        return mWord
        case mWord of
          Nothing -> do
            logErrorNS "addTranslationFromTo" "There is no such lang from in the database"
            return Nothing
          Just word -> do
            a <- insertUnique $ Word ((wordWord . entityVal) word) (entityKey langTo) ((wordPartOfSpeech . entityVal) word)
            return a
  where
    getLang = getBy . LanguageNameUnq
