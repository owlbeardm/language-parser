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


import           ClassyPrelude hiding (Word)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (LoggingT,
                                               runStderrLoggingT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Database.Esqueleto
import           Database.Language
import           Database.Word
import           Database.Persist.Postgresql (withPostgresqlConn, ConnectionString)
import           Database.Persist.TH
import           Lib


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Language sql=language_tbl
    name LanguageName
    LanguageNameUnq name
    deriving Show
Word sql=word_tbl
    word Text
    langId LanguageId
    partOfSpeech PartOfSpeech
    WordWordPosLangIdUnq word partOfSpeech langId
    deriving Show
Translation sql=translation_tbl
    fromWordId WordId
    toLangId LanguageId
    toWordId WordId Maybe
    comment Text Maybe 
    altTranslation Text Maybe 
    deriving Show
|]

connStr :: ConnectionString
connStr = "host=172.20.7.103 dbname=wiki user=wiki password=wiki port=5432"

runSQLAction :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runSQLAction = runStderrLoggingT . runResourceT . withPostgresqlConn connStr . runSqlConn


main :: IO ()
main = runSQLAction $ do
        printMigration migrateAll



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
             where_ (word ^. WordId ==. val (toSqlKey 1))
             return word
    liftIO $ mapM_ (putStrLn . tshow .  wordPartOfSpeech . entityVal) words

mainT :: IO ()
mainT = runSQLAction $ do
    trans <- select $
             from $ \tran -> do
             return tran
    liftIO $ mapM_ (print . tshow . entityVal) (trans :: [Entity Translation])

translate :: Text -> IO ()
translate wtt = runSQLAction $ do
     results <- select $
             from $ \(wf,wt,tr,tl) -> do
             return (wf,wt,tr,tl)
    liftIO $ mapM_ (print . tshow . entityVal) results