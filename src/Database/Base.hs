{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Base
  (
  LanguageName (..),
  PartOfSpeech (..),
  AppT,
  runSQLAction
  ) where

import           ClassyPrelude
import           Control.Monad.Logger         (NoLoggingT, runNoLoggingT)
-- import           Control.Monad.Logger         (LoggingT, runStderrLoggingT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Swagger                 (ToSchema)
import           Database.Esqueleto
import           Database.Persist.Postgresql  (ConnectionString,
                                               withPostgresqlConn)
import           Database.Persist.TH
import           Web.HttpApiData              (FromHttpApiData, parseQueryParam,
                                               readTextData)


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
                  | Khuzdûl
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
    deriving (Eq, Enum, Read, Show, Generic, ToJSON, FromJSON, ToSchema)
derivePersistField "LanguageName"

instance FromHttpApiData LanguageName where
  parseQueryParam = readTextData

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
-- connStr = "host=172.19.7.103 dbname=wiki user=wiki password=wiki port=5432"
connStr = "host=kedom.cql8wtdso3sc.eu-central-1.rds.amazonaws.com dbname=wiki user=wiki password=H1UniO7Nz7QeNqg6T9xa port=5432"

runSQLAction :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction =
  runNoLoggingT . runResourceT . withPostgresqlConn connStr . runSqlConn
