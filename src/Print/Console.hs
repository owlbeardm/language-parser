module Print.Console where

import           ClassyPrelude                             hiding (words, (<>))
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Language
import           Database.Translation
import           Database.Word

-- printList :: (a->Text) -> IO [a] -> IO ()
-- printList fshow list = do
--      e <- list
--      putStrLn $ fshow e

printLangs :: IO ()
printLangs = runSQLAction $ do
     langs <- listLangs
     liftIO $ mapM_ (putStrLn . tshow . entityVal) langs

printAllTranslations :: LanguageName -> IO ()
printAllTranslations lname = do
     trans <- printAllTranslationsByLang lname
     mapM_ ( putStrLn . tshowFT) trans

printWordsFrom :: LanguageName -> IO ()
printWordsFrom langName = runSQLAction $ do
     words <- listWordsByLang langName
     liftIO $ mapM_ (putStrLn . tshow . wordWord . entityVal) words
     putStr "\n\tTotal: "
     print $ length words

printTranslate :: Text -> IO ()
printTranslate wtt = do
  tranl <- translate wtt
  mapM_ ( putStrLn . tshowFT) tranl


tshowFT :: FullTranslation -> Text
tshowFT (tr, frWord, frLang, toLang, mToWord) = renderStrict . layoutPretty defaultLayoutOptions $
      annotate (color Blue) (case mToWord of
            Just toWord -> tshowWord toWord
            _ ->
              case translationAltTranslation tr of
                Just a -> pretty a
                _      -> "")
      <+> annotate (color Black) (tshowLang toLang
                                  <+> annotate (color Green) (tshowWord frWord)
                                  <+> tshowLang frLang)
      <+> annotate (color Black) (pretty $ translationComment tr)
     where
      tshowLang lang = "(" <> annotate italicized "from" <+> pretty (tshow lang) <> ")"
      tshowWord word = pretty (wordWord word)
           <+> annotate (color Black) ("["
                                           <> pretty (conShow $ wordPartOfSpeech word)
                                           <> "]")


conShow :: PartOfSpeech -> Text
conShow Adjective   = "adj."
conShow Adverb      = "adv."
conShow Conjunction = "cnj."
conShow Determiner  = "det."
conShow Noun        = "n."
conShow Numeral     = "num."
conShow Preposition = "prep."
conShow Pronoun     = "pn."
conShow Verb        = "v."
conShow a           = tshow a

--       mconcat
--         [ case mToWord of
--             Just toWord -> showWord toWord
--             _ ->
--               case translationAltTranslation tr of
--                 Just a -> a
--                 _      -> ""
--         , "\x1b[2m ("
--         , tshow toLang
--         , ") "
--         , fromPosLength mToWord (translationAltTranslation tr)
--         ,"[from ("
--         , tshow frLang
--         , ") \x1b[0m"
--         , showWord frWord
--         , "\x1b[2m] "
--         , case translationComment tr of
--                 Just a -> mconcat ["\t\t\"",a,"\""]
--                 _      -> ""
--         ,"\x1b[0m"
--         ]
--   where
--     fromPosLength Nothing (Just t) = if length t > 12 then "\t" else "\t\t"
--     fromPosLength (Just w) _ = if length (mconcat [wordWord w, conShow $ wordPartOfSpeech w]) > 11 then "\t" else "\t\t"


-- (\c -> putDoc ((annotate (color  c) b1) <+> (annotate (colorDull  c) b1))) Black
-- (\c -> putDoc ((annotate (color  c) b1) <+> (annotate (colorDull  c) b1))) Red
-- (\c -> putDoc ((annotate (color  c) b1) <+> (annotate (colorDull  c) b1))) Green
-- (\c -> putDoc ((annotate (color  c) b1) <+> (annotate (colorDull  c) b1))) Yellow
-- (\c -> putDoc ((annotate (color  c) b1) <+> (annotate (colorDull  c) b1))) Blue
-- (\c -> putDoc ((annotate (color  c) b1) <+> (annotate (colorDull  c) b1))) Magenta
-- (\c -> putDoc ((annotate (color  c) b1) <+> (annotate (colorDull  c) b1))) Cyan
-- (\c -> putDoc ((annotate (color  c) b1) <+> (annotate (colorDull  c) b1))) White

