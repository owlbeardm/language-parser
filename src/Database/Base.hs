{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Database.Base
  (
  LanguageName (..),
  PartOfSpeech (..),
  WordOriginType (..),
  AppT,
  runSQLAction,
  runDb
  ) where

import           ClassyPrelude
import           Control.Monad.Logger         (LoggingT, runStderrLoggingT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Swagger                 (ToParamSchema (..),
                                               ToSchema (..))
import           Database.Esqueleto
import           Database.Persist.Postgresql  (ConnectionString,
                                               withPostgresqlConn)
import           Database.Persist.TH
import           HTTP.Config                  hiding (connStr)
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
                  | ProtoOrc
                  | ProtoTengu
                  | Queran
                  | SlaveRunic
                  | Sylvan
                  | Titan
    deriving (Eq, Enum, Read, Show, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
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
    deriving (Eq, Bounded, Enum, Read, Show, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
derivePersistField "PartOfSpeech"

data WordOriginType = Evolved | Migrated | Combined | Derivated
  deriving (Eq, Bounded, Enum, Read, Show, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

connStr :: ConnectionString
connStr = "host=172.19.7.103 dbname=wiki user=wiki password=wiki port=5432"

-- runSQLAction :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runSQLAction =
  -- runNoLoggingT . runResourceT . withPostgresqlConn connStr . runSqlConn
  runStderrLoggingT . runResourceT . withPostgresqlConn connStr . runSqlConn

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
