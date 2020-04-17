module Database.Word where

import           ClassyPrelude                hiding (Word, delete, groupBy,
                                               isNothing, on)
import           Control.Monad.Logger
import           Database.Base
import           Database.Entity
import           Database.Esqueleto

addWord :: (MonadIO m, MonadLogger m) => WordText -> PartOfSpeech -> LanguageName -> AppT m (Maybe (Key Word))
addWord word pos langName = do
  lang <- getBy $ LanguageNameUnq langName
  case lang of
    Nothing -> do
      logErrorNS "addWord" "There is no such lang in the database"
      return Nothing
    (Just l) -> insertUnique $ Word word (entityKey l) pos False

listWordsByLang :: (MonadIO m) => LanguageName -> AppT m [Entity Word]
listWordsByLang langName = select $ from $ \(word,lang) -> do
      where_ (word ^. WordLangId ==. lang ^. LanguageId &&.
              lang ^. LanguageLname ==. val langName )
      return word

listNotEvolvedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity Word]
listNotEvolvedWordsByLangFromAndTo langNameFrom langNameTo =
  select $
  from $ \((word `InnerJoin` langFrom) `LeftOuterJoin` (wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo)) -> do
      on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
      on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
      on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
      on (word ^. WordId ==. wordOrgFrom ^. WordOriginFromWordFromId)
      on (word ^. WordLangId ==. langFrom ^. LanguageId)
      where_ (langFrom ^. LanguageLname ==. val langNameFrom &&.
              (isNothing (just (wordOrgFrom ^. WordOriginFromWordFromId)) ||.
              langTo ^. LanguageLname !=. val langNameTo))
      groupBy (word ^. WordId)
      return word

deleteEvolvedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m ()
deleteEvolvedWordsByLangFromAndTo langNameFrom langNameTo =
  delete $
  from $ \((word `InnerJoin` langFrom) `InnerJoin` (wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo)) -> do
      on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
      on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
      on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
      on (word ^. WordId ==. wordOrgFrom ^. WordOriginFromWordFromId)
      on (word ^. WordLangId ==. langFrom ^. LanguageId)
      where_ (langFrom ^. LanguageLname ==. val langNameFrom &&.
              langTo ^. LanguageLname !=. val langNameTo)


getWord :: (MonadIO m, MonadLogger m) =>
     WordText
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
        mWord <- getBy $ WordWordPosLangIdUnq wordText posWord (entityKey langFrom)
        case mWord of
          Nothing -> do
            logErrorNS "addTranslationFromTo" "There is no such lang from in the database"
            return Nothing
          Just word -> insertUnique $ Word ((wordWord . entityVal) word) (entityKey langTo) ((wordPartOfSpeech . entityVal) word) False
  where
    getLang = getBy . LanguageNameUnq
