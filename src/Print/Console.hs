module Print.Console where

import           ClassyPrelude                             hiding (words, (<>), keys, Word)
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


tshowWord :: Word -> Text
tshowWord = tshowPretty prettyWord 

tshowPretty :: (a -> Doc AnsiStyle) -> a -> Text
tshowPretty prettS value = renderStrict . layoutPretty defaultLayoutOptions $ prettS value

prettyWordDescription :: WordDescription -> Doc AnsiStyle
prettyWordDescription (word, langs, trans) = vsep [
               "",
               prettyWord word <+> printLangList langs,
               "",
               "       " <+> align (vsep (map prettyWT trans)),
               ""]
     where
          printLangList ls = annotate (color Black) (
               annotate italicized "from"
               <+> hsep (punctuate comma (map (annotate bold . pretty . tshow) ls)))     

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