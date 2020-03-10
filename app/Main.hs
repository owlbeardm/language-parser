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
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Database.Language
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Quasi
import           Database.Persist.TH
import           Language.Haskell.TH.Quote
import           Lib


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Language sql=language_tbl
    name LanguageName
    LanguageNameUnq name
    deriving Show
Word sql=word_tbl
    word Text
    langId LanguageId
    partOfSpeech Text
    WordWordPosLangIdUnq word partOfSpeech langId
    deriving Show
|]

connStr :: ConnectionString
connStr = "host=172.20.7.103 dbname=wiki user=wiki password=wiki port=5432"

runSQLAction :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runSQLAction = runStderrLoggingT . runResourceT . withPostgresqlConn connStr . runSqlConn


-- main :: IO ()
-- main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $
--     flip runSqlPersistMPool pool $ do
--         printMigration migrateAll
--         lang <- getBy $ LanguageNameUnq English
--         liftIO $ print lang

main :: IO ()
main = runSQLAction $ do
        printMigration migrateAll
        lang <- getBy $ LanguageNameUnq English
        liftIO $ print lang
        word <- getBy $ WordWordPosLangIdUnq "ask" "verb" (toSqlKey 3)
        liftIO $ print word

-- getWordById :: IO ()
-- getWordById = runSQLAction $ do
--         word <- get $ (toSqlKey 3) :: Word
--         liftIO $ print word