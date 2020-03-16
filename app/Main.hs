{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}




import           ClassyPrelude                hiding (Word, on, (<&>))
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (LoggingT, NoLoggingT, logErrorN,
                                               logErrorNS, runNoLoggingT,
                                               runStderrLoggingT)
import           Control.Monad.Trans.Maybe    (mapMaybeT, runMaybeT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Monoid                  (mconcat)
import           Database.Esqueleto
import           Database.Language
import           Database.Persist.Postgresql  (ConnectionString,
                                               withPostgresqlConn)
import           Database.Persist.TH
import           Database.Word
import           Lib
import           Web.Scotty                   as S




share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Language sql=language_tbl
    lname LanguageName sql=name
    LanguageNameUnq lname
Word sql=word_tbl
    word Text
    langId LanguageId
    partOfSpeech PartOfSpeech
    WordWordPosLangIdUnq word partOfSpeech langId
Translation sql=translation_tbl
    fromWordId WordId
    toLangId LanguageId
    toWordId WordId Maybe
    comment Text Maybe
    altTranslation Text Maybe
    deriving Show
|]

instance Show Language where
    show l = show $ languageLname l

instance Show Word where
    show w = mconcat [ unpack $ wordWord w, " '", show $ wordPartOfSpeech w, "'"]

type FullTranslation = (Translation, Word, Language, Language, Maybe Word)

connStr :: ConnectionString
connStr = "host=172.20.7.103 dbname=wiki user=wiki password=wiki port=5432"


runSQLAction :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runSQLAction = runStderrLoggingT . runResourceT . withPostgresqlConn connStr . runSqlConn

-- main :: IO ()
-- main = scotty 3000 $
--     S.get "/:word" $ do
--         beam <- param "word"
--         html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


main :: IO ()
main = runSQLAction $ do
        printMigration migrateAll
        action

action :: ReaderT SqlBackend (ResourceT (LoggingT IO)) ()
action = do
    langs <- select $
             from $ \lang -> do
             return lang
    liftIO $ mapM_ (print . languageLname . entityVal) langs

mainI :: IO ()
mainI = runSQLAction $ do
    langs <- select $
             from $ \lang -> do
             return lang
    liftIO $ mapM_ (print . languageLname . entityVal) langs

mainW :: IO ()
mainW = runSQLAction $ do
    words <- select $
             from $ \word -> do
             where_ (word ^. WordId ==. val (toSqlKey 15))
             return word
    liftIO $ mapM_ (putStrLn . tshow .  entityVal) words

mainUnq :: Text ->  PartOfSpeech -> LanguageName -> IO ()
mainUnq word pos langName = runSQLAction $ do
    lang <- getBy $ LanguageNameUnq langName
    case lang of
        Just l -> do
            mWord <- getBy $ WordWordPosLangIdUnq word pos (entityKey l)
            liftIO $ mapM_ (putStrLn . tshow .  entityVal) mWord
        Nothing -> liftIO $ print lang

mainT :: IO ()
mainT = runSQLAction $ do
    trans <- select $
             from $ \tran -> do
             return tran
    liftIO $ mapM_ (print . tshow . entityVal) (trans :: [Entity Translation])


translate :: Text -> IO ()
translate wtt = runSQLAction $ do
    results <- select $
             from $ \(tr `InnerJoin` frWord `InnerJoin` frLang `InnerJoin` toLang `LeftOuterJoin` mToWord) -> do
             on (tr ^. TranslationToWordId ==. mToWord ?. WordId)
             on (tr ^. TranslationToLangId ==. toLang ^. LanguageId)
             on (frWord ^. WordLangId ==. frLang ^. LanguageId)
             on (tr ^. TranslationFromWordId ==. frWord ^. WordId)
             where_ ((frWord ^. WordWord ==. val (toLower wtt))
               ||. (mToWord ?. WordWord ==. just(val (toLower wtt)))
               ||. (tr ^. TranslationAltTranslation `like` just (val (toLower (mconcat ["%",wtt,"%"])))))
             return (tr, frWord, frLang, toLang, mToWord)
    liftIO $ mapM_ (putStrLn . showFT . convert) results
    where
     convert (tr, frWord, frLang, toLang, mToWord) = (entityVal tr, entityVal frWord, entityVal frLang, entityVal toLang, map entityVal mToWord)
     showFT (tr, frWord, frLang, toLang, mToWord) = mconcat [
         case mToWord of
          Just toWord -> tshow toWord
          _ -> case translationAltTranslation tr of
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

addLang :: LanguageName -> IO ()
addLang name = runSQLAction . void . insert $ Language name

addWord :: Text -> PartOfSpeech -> LanguageName -> IO ()
addWord word pos langName = runSQLAction $ do
    lang <- getBy $ LanguageNameUnq langName
    case lang of
        Just l -> do
            id <- insert $ Word word ( entityKey l ) pos
            liftIO $ print id
        Nothing -> logErrorNS "addWord" "There is no such lang in the database"

addTranslationFromTo :: Text ->  PartOfSpeech -> LanguageName -> Text -> PartOfSpeech -> LanguageName -> Maybe Text -> IO ()
addTranslationFromTo fromWord fromPos fromLang toWord toPos toLang mComment = runSQLAction $ do
    mLangFrom <- getLang fromLang
    mLangTo <- getLang toLang
    case (mLangFrom,mLangTo) of
        (Nothing,_) -> logErrorNS logLabel "There is no such lang to in the database"
        (_,Nothing) -> logErrorNS logLabel "There is no such lang from in the database"
        (Just langFrom,Just langTo)-> do
            mWordFrom <- getBy $ WordWordPosLangIdUnq fromWord fromPos (entityKey langFrom)
            mWordTo <- getBy $ WordWordPosLangIdUnq toWord toPos (entityKey langTo)
            case (mWordFrom,mWordTo) of
                 (Nothing,_) -> logErrorNS logLabel "There is no such word to in the database"
                 (_,Nothing) -> logErrorNS logLabel "There is no such word from in the database"
                 (Just wordFrom,Just wordTo)-> do
                     id <- insert $ Translation (entityKey wordFrom) (entityKey langTo) (Just (entityKey wordTo)) mComment Nothing
                     liftIO $ print id
    where
        getLang = getBy . LanguageNameUnq
        logLabel = "addTranslationFromTo"
