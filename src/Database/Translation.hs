module Database.Translation where

import           ClassyPrelude hiding (on)
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import          Control.Monad.Logger

translate :: Text -> IO ()
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
          ((frWord ^. WordWord ==. val (toLower wtt)) ||.
           (mToWord ?. WordWord ==. just (val (toLower wtt))) ||.
           (tr ^. TranslationAltTranslation `like`
            just (val (toLower (mconcat ["%", wtt, "%"])))))
        return (tr, frWord, frLang, toLang, mToWord)
    liftIO $ mapM_ (putStrLn . showFT . convert) results
  where
    convert (tr, frWord, frLang, toLang, mToWord) =
      ( entityVal tr
      , entityVal frWord
      , entityVal frLang
      , entityVal toLang
      , map entityVal mToWord)
    showFT (tr, frWord, frLang, toLang, mToWord) =
      mconcat
        [ case mToWord of
            Just toWord -> tshow toWord
            _ ->
              case translationAltTranslation tr of
                Just a -> a
                _      -> ""
        , " ("
        , tshow toLang
        , ") [from ("
        , tshow frLang
        , ") "
        , tshow frWord
        , "]"
        ]


addTranslationFromTo :: (MonadIO m, MonadLogger m) =>
     Text
  -> PartOfSpeech
  -> LanguageName
  -> Text
  -> PartOfSpeech
  -> LanguageName
  -> Maybe Text
  -> AppT m (Maybe (Key Translation))
addTranslationFromTo fromWord fromPos fromLang toWord toPos toLang mComment = do
    mLangFrom <- getLang fromLang
    mLangTo <- getLang toLang
    case (mLangFrom, mLangTo) of
      (Nothing, _) ->  do 
        logErrorNS "addTranslationFromTo" "There is no such lang to in the database"
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
            logErrorNS "addTranslationFromTo" "There is no such lang to in the database"
            return Nothing
          (_, Nothing) ->  do 
            logErrorNS "addTranslationFromTo" "There is no such lang to in the database"
            return Nothing
          (Just wordFrom, Just wordTo) -> do
            id <- insert $ Translation (entityKey wordFrom) (entityKey langTo) (Just (entityKey wordTo)) mComment Nothing
            return (Just id)
  where
    getLang = getBy . LanguageNameUnq