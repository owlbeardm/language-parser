module Database.Translation where

import           ClassyPrelude        hiding (groupBy, id, on, words)
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
type WordDescription = (Database.Entity.Word, [Language], [WordTranslation], [WordSource])
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
    return $ zip4 (map entityVal words) (map (map entityVal) langs) translations (zipOrigins origins shortTranslations)
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
getWordTranslations enWord = do
    results <-
      select $
      from $ \(tr `InnerJoin` frWord `InnerJoin` toLang `LeftOuterJoin` mToWord) -> do
        on (tr ^. TranslationToWordId ==. mToWord ?. WordId)
        on (tr ^. TranslationToLangId ==. toLang ^. LanguageId)
        on (tr ^. TranslationFromWordId ==. frWord ^. WordId)
        where_
          (frWord ^. WordId ==. val (entityKey enWord))
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


addTranslationFromTo :: (MonadIO m, MonadLogger m) =>
     WordText
  -> PartOfSpeech
  -> LanguageName
  -> WordText
  -> PartOfSpeech
  -> LanguageName
  -> Maybe Comment
  -> AppT m (Maybe (Key Translation))
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
          (Just wordFrom, Just wordTo) -> do
            id <- insert $ Translation (entityKey wordFrom) (entityKey langTo) (Just (entityKey wordTo)) mComment Nothing
            return (Just id)
  where
    getLang = getBy . LanguageNameUnq


addTranslationFromAlt :: (MonadIO m, MonadLogger m) =>
     WordText
  -> PartOfSpeech
  -> LanguageName
  -> LanguageName
  -> Maybe Comment
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

