{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HTTP.Config where

import           ClassyPrelude               hiding (keys)
import           Control.Monad.Except
import           Control.Monad.Logger
import qualified Data.ByteString.Char8       as BS
import           Database.Esqueleto
import           Database.Persist.Postgresql (ConnectionString,
                                              createPostgresqlPool)
import           Network.Wai.Handler.Warp
import           Control.Monad.Trans.Maybe
import           Servant.Server
import           System.Environment

newtype AppServerT m a
    = AppServerT
    { runApp :: ReaderT Config (ExceptT ServerError m) a
    } deriving
    ( Functor, Applicative, Monad, MonadReader Config, MonadError ServerError
    , MonadIO
    )

data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

data Config
    = Config
    {
      configPool :: ConnectionPool
    , configEnv  :: Environment
    -- , configMetrics   :: Metrics
    -- , configEkgServer :: ThreadId
    -- , configLogEnv    :: LogEnv
    , configPort :: Port
    } deriving (Show)

getConfig :: IO Config
getConfig = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8000
    pool <- makePool env
    return $ Config pool env port

makePool :: Environment -> IO ConnectionPool
makePool Development = runNoLoggingT $ createPostgresqlPool (connStr "") (envPool Development)
makePool Test        = runNoLoggingT $ createPostgresqlPool (connStr "") (envPool Test)
makePool Production  = do
    pool <- runMaybeT $ do
        let keys = [ "host="
                   , "port="
                   , "user="
                   , "password="
                   , "dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
        lift $ runNoLoggingT $ createPostgresqlPool prodStr (envPool Production)
    case pool of
         Nothing -> throwIO (userError "Database Configuration not present in environment.")
         Just a -> return a


-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=172.19.7.103 dbname=wiki" <> sfx <> " user=wiki password=wiki port=5432"

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
