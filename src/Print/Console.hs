module Print.Console where

import           ClassyPrelude                             hiding (Word, keys,
                                                            words, (<>))
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Language
import           Database.Translation
import           Database.Word

printLangs :: IO ()
printLangs = runSQLAction $ do
     langs <- listLangs
     liftIO $ mapM_ (putStrLn . tshow . entityVal) langs

printWordsFrom :: LanguageName -> IO ()
printWordsFrom langName = runSQLAction $ do
     words <- listWordsByLang langName
     fullDescr <- getFullWordDescription words
     liftIO $ mapM_ (putStrLn . tshowPretty prettyWordDescription) fullDescr
     putStr "\n\tTotal: "
     print $ length words

printLookupWord :: Text -> IO ()
printLookupWord text = runSQLAction $ do
     words <- findWordsByText text
     fullDescr <- getFullWordDescription words
     liftIO $ mapM_ (putStrLn . tshowPretty prettyWordDescription) fullDescr

printTranslate :: Text -> IO ()
printTranslate translationText = runSQLAction $ do
  translations <- translateWord translationText
  mapM_ ( putStrLn . tshowPretty prettyWordDescription) translations

printNotEvolvedWordsFrom :: LanguageName -> LanguageName -> IO ()
printNotEvolvedWordsFrom langName1 langName2 = runSQLAction $ do
     words <- listNotEvolvedWordsByLangFromAndTo langName1 langName2
     liftIO $ mapM_ (putStrLn . tshow . wordWord . entityVal) words
     putStr "\n\tTotal: "
     print $ length words

printEvolveLaws :: LanguageName -> LanguageName -> IO ()
printEvolveLaws langName1 langName2 = runSQLAction $ do
     laws <- listEvolveLawsByLangs langName1 langName2
     liftIO $ mapM_ (putStrLn . tshow . entityVal) laws
     putStr "\n\tTotal: "
     print $ length laws

cEvolveLangs :: LanguageName -> LanguageName -> IO ()
cEvolveLangs langName1 langName2 = runSQLAction $ do
     keys <- evolveLang langName1 langName2
     liftIO $ mapM_ (putStrLn . tshow) keys
     putStr "\n\tTotal: "
     print $ length keys



cReEvolveLangs :: LanguageName -> LanguageName -> IO ()
cReEvolveLangs langName1 langName2 = runSQLAction $ do
     keys <- reEvolveLang langName1 langName2
     case keys of
          Just ks -> do
                mapM_ (putStrLn . tshow) ks
                putStr "\n\tTotal: "
                print $ length ks
          _ -> putStrLn "Nothing"

tshowWord :: Word -> Text
tshowWord = tshowPretty prettyWord

tshowPretty :: (a -> Doc AnsiStyle) -> a -> Text
tshowPretty prettS value = renderStrict . layoutPretty defaultLayoutOptions $ prettS value

prettyWordDescription :: WordDescription -> Doc AnsiStyle
prettyWordDescription (word, langs, trans, wordAndLangsOrigins) = vsep [
               "",
               prettyWord word <+> printLangList langs,
               if null trans then mempty else vsep ["", "       " <+> align (vsep (map prettyWT trans))],
               if null wordAndLangsOrigins 
                    then mempty 
                    else annotate (color Black) $ vsep ["", "       " <+> align (hsep (punctuate semi (map prettyWordSource wordAndLangsOrigins)))],
               ""]
     where
          printLangList ls = annotate (color Black) (
               annotate italicized "from"
               <+> hsep (punctuate comma (map (annotate bold . pretty . tshow) ls)))

prettyShortTranslation :: WordTranslation -> Doc AnsiStyle
prettyShortTranslation (translation, _, mWord) = annotate (colorDull Blue) (
     case mWord of
          Just word -> pretty (wordWord word)
          _ -> case translationAltTranslation translation of
               Just alt -> pretty alt
               _ -> mempty)

prettyWordSource :: WordSource -> Doc AnsiStyle
prettyWordSource (wordAndLang, translations) = 
     prettyWordAndLang wordAndLang
     <+> if null translations
          then mempty
          else (parens . hsep . punctuate comma . map prettyShortTranslation) translations

prettyWordAndLang :: WordAndLang -> Doc AnsiStyle
prettyWordAndLang (word, lang) = annotate (color Black) (
     annotate italicized "from"
     <+> annotate bold (pretty (tshow lang))
     <+> annotate (colorDull Green) ( "/" <+> pretty (wordWord word) <+> "/"))

prettyWord :: Word -> Doc AnsiStyle
prettyWord word =
     ""
     <+> annotate (color Green) ( "/" <+> pretty (wordWord word) <+> "/")
     <+> annotate (color Black) (brackets $ annotate bold $ pretty (conShow $ wordPartOfSpeech word))

prettyWT :: WordTranslation -> Doc AnsiStyle
prettyWT (translation, toLang, mToWord) =
          annotate (color Blue) (case mToWord of
               Just toWord -> pretty (wordWord toWord)
               _ ->
                    case translationAltTranslation translation of
                         Just a -> pretty a
                         _      -> "")
          <+> ""
          <+> annotate (color Black) (
               annotate italicized "from"
               <+> annotate bold (pretty (tshow toLang))
               <+> if (null . translationComment) translation then mempty else (parens . pretty . translationComment) translation)


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
