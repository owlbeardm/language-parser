{-# LANGUAGE TemplateHaskell #-}

module Database.Base
  (
  LanguageName (..),
  PartOfSpeech (..),
  AppT,
  runSQLAction
  ) where

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
                  | ClassicalArcane
                  | Dragon
                  | Dwarven
                  | Edhellen
                  | English
                  | Halfling
                  | Infernal
                  | Kobold
                  | LizardFolk
                  | Necril
                  | Nerlendic
                  | Nitholan
                  | NitholanEmpire
                  | OldDragon
                  | OldNerlendic
                  | OldNitholan
                  | OldRunic
                  | Orkish
                  | PrimalMagic
                  | ProtoCreation
                  | ProtoDragon
                  | ProtoDwarven
                  | ProtoElven
                  | ProtoHuman
                  | ProtoMaterial
                  | ProtoMonster
                  | ProtoOrk
                  | ProtoTengu
                  | Queran
                  | SlaveRunic
                  | Sylvan
                  | Titan
    deriving (Eq, Enum, Read, Show)
derivePersistField "LanguageName"

data PartOfSpeech = Adjective
    | Adverb
    | Conjunction
    | Determiner
    | Noun
    | Numeral
    | Prefix
    | Preposition
    | Pronoun
    | Suffix
    | Verb
    deriving (Eq, Enum, Read, Show)
derivePersistField "PartOfSpeech"

connStr :: ConnectionString
connStr = "host=localhost dbname=wiki user=wiki password=wiki port=5432"

runSQLAction :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction =
  runNoLoggingT . runResourceT . withPostgresqlConn connStr . runSqlConn


