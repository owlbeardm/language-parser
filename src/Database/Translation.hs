module Database.Translation where

import           ClassyPrelude hiding (on)
import           Database.Base
import           Database.Entity
import           Database.Esqueleto

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