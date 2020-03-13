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




import           ClassyPrelude hiding (Word, on)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (LoggingT,
                                               runStderrLoggingT,
                                               NoLoggingT,
                                               runNoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Database.Esqueleto
import           Database.Language
import           Database.Word
import           Database.Persist.Postgresql (withPostgresqlConn, ConnectionString)
import           Database.Persist.TH
import           Lib
import Web.Scotty as S
import Data.Monoid (mconcat)




share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Language sql=language_tbl
    name LanguageName
    LanguageNameUnq name
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
    show l = show $ languageName l

instance Show Word where
    show w = mconcat [ unpack $ wordWord w, " '", show $ wordPartOfSpeech w, "'"]

type FullTranslation = (Translation, Word, Language, Language, Maybe Word)

connStr :: ConnectionString
connStr = "host=172.20.7.103 dbname=wiki user=wiki password=wiki port=5432"


runSQLAction :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction = runNoLoggingT . runResourceT . withPostgresqlConn connStr . runSqlConn

main :: IO ()
main = scotty 3000 $
    S.get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


main' :: IO ()
main' = runSQLAction $ do
        printMigration migrateAll
        action

action :: ReaderT SqlBackend (ResourceT (NoLoggingT IO)) ()
action = do
    langs <- select $
             from $ \lang -> do
             return lang
    liftIO $ mapM_ (print . languageName . entityVal) langs

mainI :: IO ()
mainI = runSQLAction $ do
    langs <- select $
             from $ \lang -> do
             return lang
    liftIO $ mapM_ (print . languageName . entityVal) langs

mainW :: IO ()
mainW = runSQLAction $ do
    words <- select $
             from $ \word -> do
             where_ (word ^. WordId ==. val (toSqlKey 15))
             return word
    liftIO $ mapM_ (putStrLn . tshow .  entityVal) words

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
     convert = \(tr, frWord, frLang, toLang, mToWord) -> (entityVal tr, entityVal frWord, entityVal frLang, entityVal toLang, map entityVal mToWord)
     showFT = \(tr, frWord, frLang, toLang, mToWord) -> mconcat [
         case mToWord of 
          Just toWord -> tshow toWord
          _ -> case translationAltTranslation tr of 
           Just a -> a
           _ -> ""
         , " ("
         , tshow toLang
         , ") [from ("
         , tshow frLang
         , ") "
         , tshow frWord
         , "]"
         ]

    -- fromWordId WordId
    -- toLangId LanguageId
    -- toWordId WordId Maybe
    -- comment Text Maybe 
    -- altTranslation Text Maybe 
-- (tr, frLang, frWord, toLang, mToWord)
-- (Entity (Translation, Language, Word, Language, Maybe Word))
    -- from $ \(b, p) -> do
    -- from $ \(p `LeftOuterJoin` mb) -> do