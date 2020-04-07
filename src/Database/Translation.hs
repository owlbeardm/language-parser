module Database.Translation where

import           ClassyPrelude        hiding (on)
import           Control.Monad.Logger
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Word

type FullTranslation = (Translation, Database.Entity.Word, Language, Language, Maybe Database.Entity.Word)

translate :: Text -> IO [FullTranslation]
translate wtt =
  runSQLAction $ do
    results <-
      select $
      from $ \(tr `InnerJoin` frWord `InnerJoin` frLang `InnerJoin` toLang `LeftOuterJoin` mToWord) -> do
        on (tr ^. TranslationToWordId ==. mToWord ?. WordId)
        on (tr ^. TranslationToLangId ==. toLang ^. LanguageId)
        on (frWord ^. WordLangId ==. frLang ^. LanguageId)
        on (tr ^. TranslationFromWordId ==. frWord ^. WordId)
        where_
          ((frWord ^. WordWord ==. val wtt) ||.
           (mToWord ?. WordWord ==. just (val wtt)) ||.
           (tr ^. TranslationAltTranslation `like`
            just (val (mconcat ["%", wtt, "%"]))))
        orderBy [asc (toLang ^. LanguageId), asc (mToWord ?. WordWord), asc (tr ^. TranslationAltTranslation), asc (frWord ^. WordWord)]
        return (tr, frWord, frLang, toLang, mToWord)
    return $ map convert results
  where
    convert (tr, frWord, frLang, toLang, mToWord) =
      ( entityVal tr
      , entityVal frWord
      , entityVal frLang
      , entityVal toLang
      , map entityVal mToWord)


printAllTranslationsByLang :: LanguageName -> IO [FullTranslation]
printAllTranslationsByLang lname =
  runSQLAction $ do
    results <-
      select $
      from $ \(tr `InnerJoin` frWord `InnerJoin` frLang `InnerJoin` toLang `LeftOuterJoin` mToWord) -> do
        on (tr ^. TranslationToWordId ==. mToWord ?. WordId)
        on (tr ^. TranslationToLangId ==. toLang ^. LanguageId)
        on (frWord ^. WordLangId ==. frLang ^. LanguageId)
        on (tr ^. TranslationFromWordId ==. frWord ^. WordId)
        where_
          (frLang ^. LanguageLname ==. val lname)
        orderBy [asc (toLang ^. LanguageId), asc (mToWord ?. WordWord), asc (tr ^. TranslationAltTranslation), asc (frWord ^. WordWord)]
        return (tr, frWord, frLang, toLang, mToWord)
    return $ map convert results
  where
    convert (tr, frWord, frLang, toLang, mToWord) =
      ( entityVal tr
      , entityVal frWord
      , entityVal frLang
      , entityVal toLang
      , map entityVal mToWord)


addTranslationFromTo :: Text
  -> PartOfSpeech
  -> LanguageName
  -> Text
  -> PartOfSpeech
  -> LanguageName
  -> Maybe Text
  -> IO (Maybe (Key Translation))
addTranslationFromTo fromWord fromPos fromLang toWord toPos toLang mComment = runSQLAction $ do
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
        mWordFrom <-
          getBy $ WordWordPosLangIdUnq fromWord fromPos (entityKey langFrom)
        mWordTo <- getBy $ WordWordPosLangIdUnq toWord toPos (entityKey langTo)
        case (mWordFrom, mWordTo) of
          (Nothing, _) ->  do
            logErrorNS "addTranslationFromTo" "There is no such word from in the database"
            return Nothing
          (_, Nothing) ->  do
            logErrorNS "addTranslationFromTo" "There is no such word to in the database"
            return Nothing
          (Just wordFrom, Just wordTo) -> do
            id <- insert $ Translation (entityKey wordFrom) (entityKey langTo) (Just (entityKey wordTo)) mComment Nothing
            return (Just id)
  where
    getLang = getBy . LanguageNameUnq


addTranslationFromAlt :: (MonadIO m, MonadLogger m) =>
     Text
  -> PartOfSpeech
  -> LanguageName
  -> LanguageName
  -> Maybe Text
  -> Text
  -> AppT m (Maybe (Key Translation))
addTranslationFromAlt fromWord fromPos fromLang toLang mComment altTran = do
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
        mWordFrom <-
          getBy $ WordWordPosLangIdUnq fromWord fromPos (entityKey langFrom)
        case mWordFrom of
          Nothing ->  do
            logErrorNS "addTranslationFromTo" "There is no such word from in the database"
            return Nothing
          Just wordFrom -> do
            id <- insert $ Translation (entityKey wordFrom) (entityKey langTo) Nothing mComment (Just altTran)
            return (Just id)
  where
    getLang = getBy . LanguageNameUnq

