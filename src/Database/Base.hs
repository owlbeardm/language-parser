{-# LANGUAGE TemplateHaskell #-}

module Database.Base where

-- import           Control.Monad.Logger         (NoLoggingT, logErrorN,
--                                                logErrorNS, runNoLoggingT,
--                                                runStderrLoggingT)
-- import           Control.Monad.Logger         (LoggingT, runStderrLoggingT)
import           ClassyPrelude
import           Control.Monad.Logger         (NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Database.Esqueleto
import           Database.Persist.Postgresql  (ConnectionString,
                                               withPostgresqlConn)
import           Database.Persist.TH


type AppT = ReaderT SqlBackend

data LanguageName = Aboleth
                  | Alko
                  | Dragon
                  | Dwarven
                  | Elven
                  | English
                  | Halfling
                  | Infernal
                  | Kobold
                  | Necril
                  | OldRunic
                  | Orkish
                  | ProtoCreation
                  | ProtoDragon
                  | ProtoDwarven
                  | ProtoElven
                  | ProtoHuman
                  | ProtoMaterial
                  | ProtoMonster
                  | ProtoOrk
                  | ProtoTengu
                  | Sylvan
                  | Titan
                  | SlaveRunic
                  | PrimalMagic
                  | OldDragon
                  | LizardFolk
    deriving (Eq, Enum, Read, Show)
derivePersistField "LanguageName"

data PartOfSpeech = Adjective
    | Adverb
    | Conjunction
    | Determiner
    | Noun
    | Numeral
    | Preposition
    | Pronoun
    | Verb
    deriving (Eq, Enum, Read, Show)
derivePersistField "PartOfSpeech"

connStr :: ConnectionString
connStr = "host=172.19.6.103 dbname=wiki user=wiki password=wiki port=5432"

runSQLAction :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction =
  runNoLoggingT . runResourceT . withPostgresqlConn connStr . runSqlConn


