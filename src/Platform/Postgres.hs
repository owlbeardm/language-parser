module Platform.Postgres where

import           ClassyPrelude
import           Data.Pool
import           Database.PostgreSQL.Simple
import           System.Environment


type Env = Pool Connection

init :: IO Env
init = acquirePool

acquirePool :: IO (Pool Connection)
acquirePool = do
  envUrl <- lookupEnv "DATABASE_URL"
  let pgUrl = fromString $ fromMaybe "postgresql://localhost/realworld" envUrl
  createPool (connectPostgreSQL pgUrl) close 1 10 10
