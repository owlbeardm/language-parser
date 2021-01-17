module Database.Translation
  (
  WordAndLang,
  WordDescription,
  WordSource,
  WordTranslation,
  addAltTranslationFromId,
  addTranslationFromAlt,
  addTranslationFromIdTo,
  addTranslationFromTo,
  getFullWordDescription,
  getWordTranslationsByKey,
  translateWord,
  ) where

import           ClassyPrelude        hiding (Word, groupBy, id, on, words)
import           Control.Monad.Logger
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Language
import           Database.Word

type FullTranslation = (Translation, Database.Entity.Word, Language, Language, Maybe Database.Entity.Word)
type WordTranslation = (Translation, Language, Maybe Database.Entity.Word)
type WordAndLang = (Database.Entity.Word, Language)
type WordSource = (WordAndLang, [WordTranslation])
                  -- ([(Database.Entity.Word, Language)], [WordTranslation])
type WordDescription = (Entity Database.Entity.Word, [Language], [WordTranslation], [WordSource])
                      --  (Database.Entity.Word, [Language], [WordTranslation], [])

translate :: WordText -> IO [FullTranslation]
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
          ((frWord ^. WordWord ==. val wtt)
           ||. (mToWord ?. WordWord ==. just (val wtt)) ||.
           (tr ^. TranslationAltTranslation `like` just (val (mconcat ["% ", wtt, " %"]))))
        orderBy [asc (toLang ^. LanguageId), asc (frLang ^. LanguageId), asc (mToWord ?. WordWord), asc (tr ^. TranslationAltTranslation), asc (frWord ^. WordWord)]
        return (tr, frWord, frLang, toLang, mToWord)
    return $ map convert results
  where
    convert (tr, frWord, frLang, toLang, mToWord) =
      ( entityVal tr
      , entityVal frWord
      , entityVal frLang
      , entityVal toLang
      , map entityVal mToWord)

translateWord :: (MonadIO m) => Text -> AppT m [WordDescription]
translateWord translationText = do
  words <- findWordsByTranslation translationText
  getFullWordDescription words

getFullWordDescription :: (MonadIO m) => [Entity Database.Entity.Word] -> AppT m [WordDescription]
getFullWordDescription words = do
    translations <- mapM getWordTranslations words
    langs <- mapM (findLangByKey . wordLangId . entityVal) words
    origins <- mapM getAllWordOrigins words
    shortTranslations <- (mapM . mapM) getWordTranslationsFromOrigins origins
    return $ zip4 words (map (map entityVal) langs) translations (zipOrigins origins shortTranslations)
  where
    removeEntityFromWordAndLang (entW, entLang) = (entityVal entW, entityVal entLang)
    getWordTranslationsFromOrigins (entW, _) = getWordTranslations entW
    makeOrigins = (map . map) removeEntityFromWordAndLang
    tempZip origins = zip (makeOrigins origins)
    makeZipInside (x, y) = zip x y
    zipOrigins origins shortTranslations = map makeZipInside (tempZip origins shortTranslations)



findWordsByTranslation :: (MonadIO m) => Text -> AppT m [Entity Database.Entity.Word]
findWordsByTranslation translationText =
      select $
      from $ \(tr `InnerJoin` frWord `LeftOuterJoin` mToWord) -> do
        on (tr ^. TranslationToWordId ==. mToWord ?. WordId)
        on (tr ^. TranslationFromWordId ==. frWord ^. WordId)
        where_
          ((mToWord ?. WordWord ==. just (val translationText))
           ||. (tr ^. TranslationAltTranslation `like` just (val (mconcat ["% ", translationText, " %"])))
           ||. (tr ^. TranslationAltTranslation `like` just (val (mconcat [translationText, " %"])))
           ||. (tr ^. TranslationAltTranslation `like` just (val (mconcat ["% ", translationText])))
           )
        orderBy [asc (frWord ^. WordLangId), asc (frWord ^. WordWord)]
        groupBy (frWord ^. WordId)
        return frWord

getWordTranslations :: (MonadIO m) => Entity Database.Entity.Word -> AppT m [WordTranslation]
getWordTranslations = getWordTranslationsByKey . fromSqlKey . entityKey

getWordTranslationsByKey :: (MonadIO m) => Int64 -> AppT m [WordTranslation]
getWordTranslationsByKey key = do
    results <-
      select $
      from $ \(tr `InnerJoin` frWord `InnerJoin` toLang `LeftOuterJoin` mToWord) -> do
        on (tr ^. TranslationToWordId ==. mToWord ?. WordId)
        on (tr ^. TranslationToLangId ==. toLang ^. LanguageId)
        on (tr ^. TranslationFromWordId ==. frWord ^. WordId)
        where_
          (frWord ^. WordId ==. val (toSqlKey key))
        orderBy [asc (toLang ^. LanguageId), asc (mToWord ?. WordWord), asc (tr ^. TranslationAltTranslation)]
        return (tr, toLang, mToWord)
    return $ map convert results
  where
    convert (tr, toLang, mToWord) =
      ( entityVal tr
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

-- |The 'addTranslationFromTo' function inserts new translation to a existing word.
--
-- >>> runSQLAction $ addTranslationFromTo "kibil" Noun Khuzdûl "silver" Noun English (Just "metal")
-- Just (TranslationKey {unTranslationKey = SqlBackendKey {unSqlBackendKey = 19980}})
addTranslationFromTo :: (MonadIO m, MonadLogger m) =>
     WordText     -- ^ 'Text' of source word
  -> PartOfSpeech     -- ^ 'PartOfSpeech' of source word
  -> LanguageName     -- ^ 'LanguageName' of source word
  -> WordText     -- ^ 'Text' of translation word
  -> PartOfSpeech     -- ^ 'PartOfSpeech' of translation word
  -> LanguageName     -- ^ 'LanguageName' of translation word
  -> Maybe Comment     -- ^ 'Maybe' 'Text' of translation comment
  -> AppT m (Maybe (Key Translation))     -- ^ 'Key' 'Translation' if inserted succsesfully
addTranslationFromTo fromWord fromPos fromLang toWord toPos toLang mComment = do
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
          (Just wordFrom, Just wordTo) -> addTranslation wordFrom wordTo mComment
  where
    getLang = getBy . LanguageNameUnq

-- |The 'addTranslationFromAlt' function inserts new translation not to a word.
--
-- >>> runSQLAction $ addTranslationFromAlt "bhrentos" Noun Titan English "animal horned" Nothing
-- Just (TranslationKey {unTranslationKey = SqlBackendKey {unSqlBackendKey = 19980}})
addTranslationFromAlt :: (MonadIO m, MonadLogger m) =>
     WordText     -- ^ 'Text' of source word
  -> PartOfSpeech     -- ^ 'PartOfSpeech' of source word
  -> LanguageName     -- ^ 'LanguageName' of source word
  -> LanguageName     -- ^ 'LanguageName' of translation word
  -> Text     -- ^ 'Text' of the translation
  -> Maybe Comment     -- ^ 'Maybe' 'Text' of translation comment
  -> AppT m (Maybe (Key Translation))
addTranslationFromAlt fromWord fromPos fromLang toLang altTran mComment = do
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
          Just wordFrom -> addAltTranslation wordFrom altTran langTo mComment
  where
    getLang = getBy . LanguageNameUnq

addTranslationFromToByKeys :: (MonadIO m, MonadLogger m) =>
     Int64
  -> Int64
  -> Maybe Comment
  -> AppT m (Maybe (Key Translation))
addTranslationFromToByKeys fromKWord toKWord mComment = do
    mWFrom <- findWordById fromKWord
    mWTo <- findWordById toKWord
    case (mWFrom, mWTo) of
      (Nothing, _) ->  do
        logErrorNS "addTranslationFromToByKeys" "There is no such word from in the database"
        return Nothing
      (_, Nothing) ->  do
        logErrorNS "addTranslationFromToByKeys" "There is no such word to in the database"
        return Nothing
      (Just wFrom, Just wTo) -> addTranslation wFrom wTo mComment


addTranslation :: (MonadIO m) =>
     Entity Word
  -> Entity Word
  -> Maybe Comment
  -> AppT m (Maybe (Key Translation))
addTranslation wFrom wTo mComment = do
  id <- insert $ Translation (entityKey wFrom) ((wordLangId . entityVal) wTo) (Just (entityKey wTo)) mComment Nothing
  return (Just id)

addAltTranslation :: (MonadIO m) =>
     Entity Word
  -> Text
  -> Entity Language
  -> Maybe Comment
  -> AppT m (Maybe (Key Translation))
addAltTranslation wFrom altTran lTo mComment = do
  id <- insert $ Translation (entityKey wFrom) (entityKey lTo) Nothing mComment (Just altTran)
  return (Just id)

addTranslationFromETo :: (MonadIO m, MonadLogger m) =>
     Entity Word
  -> WordText     -- ^ 'Text' of translation word
  -> PartOfSpeech     -- ^ 'PartOfSpeech' of translation word
  -> LanguageName     -- ^ 'LanguageName' of translation word
  -> Maybe Comment     -- ^ 'Maybe' 'Text' of translation comment
  -> AppT m (Maybe (Key Translation))     -- ^ 'Key' 'Translation' if inserted succsesfully
addTranslationFromETo wFrom toWord toPos toLang mComment = do
    mLangTo <- getLang toLang
    case mLangTo of
      Nothing ->  do
        logErrorNS "addTranslationFromIdTo" "There is no such lang to in the database"
        return Nothing
      Just langTo -> do
        mWordTo <- getBy $ WordWordPosLangIdUnq toWord toPos (entityKey langTo)
        case mWordTo of
          Nothing ->  do
            logErrorNS "addTranslationFromIdTo" "There is no such word to in the database"
            return Nothing
          Just wordTo -> addTranslation wFrom wordTo mComment
  where
    getLang = getBy . LanguageNameUnq

addTranslationFromIdTo :: (MonadIO m, MonadLogger m) =>
     Int64
  -> WordText     -- ^ 'Text' of translation word
  -> PartOfSpeech     -- ^ 'PartOfSpeech' of translation word
  -> LanguageName     -- ^ 'LanguageName' of translation word
  -> Maybe Comment     -- ^ 'Maybe' 'Text' of translation comment
  -> AppT m (Maybe (Key Translation))     -- ^ 'Key' 'Translation' if inserted succsesfully
addTranslationFromIdTo wFromId toWord toPos toLang mComment = do
  mWFrom <- findWordById wFromId
  case mWFrom of
    Nothing ->  do
       logErrorNS "addTranslationFromIdTo" "There is no such word from in the database"
       return Nothing
    Just wFrom -> addTranslationFromETo wFrom toWord toPos toLang mComment
    

addAltTranslationFromE :: (MonadIO m, MonadLogger m) =>
     Entity Word
  -> LanguageName     -- ^ 'LanguageName' of translation word
  -> Text     -- ^ 'Text' of the translation
  -> Maybe Comment     -- ^ 'Maybe' 'Text' of translation comment
  -> AppT m (Maybe (Key Translation))
addAltTranslationFromE wFrom toLang altTran mComment = do
    mLangTo <- getLang toLang
    case mLangTo of
      Nothing ->  do
        logErrorNS "addAltTranslationFromId" "There is no such lang to in the database"
        return Nothing
      Just langTo -> addAltTranslation wFrom altTran langTo mComment
  where
    getLang = getBy . LanguageNameUnq

addAltTranslationFromId :: (MonadIO m, MonadLogger m) =>
     Int64
  -> LanguageName     -- ^ 'LanguageName' of translation word
  -> Text     -- ^ 'Text' of the translation
  -> Maybe Comment     -- ^ 'Maybe' 'Text' of translation comment
  -> AppT m (Maybe (Key Translation))
addAltTranslationFromId wFromId toLang altTran mComment = do
  mWFrom <- findWordById wFromId
  case mWFrom of
    Nothing ->  do
       logErrorNS "addTranslationFromIdTo" "There is no such word from in the database"
       return Nothing
    Just wFrom -> addAltTranslationFromE wFrom toLang altTran mComment