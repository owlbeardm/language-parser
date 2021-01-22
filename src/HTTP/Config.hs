{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HTTP.Config where

import           ClassyPrelude
import           Control.Monad.Except
import           Control.Monad.Logger
-- import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8       as BS
import           Database.Esqueleto
import           Database.Persist.Postgresql (ConnectionString,
                                              createPostgresqlPool)
import           Network.Wai.Handler.Warp
import           Servant.Server
-- import           System.Environment

newtype AppServerT m a
    = AppServerT
    { runApp :: ReaderT Config (ExceptT ServerError m) a
    } deriving
    ( Functor, Applicative, Monad, MonadReader Config, MonadError ServerError
    , MonadIO
    )

data Config
    = Config
    {
      configPool :: ConnectionPool
    -- , configEnv  :: Environment
    -- , configMetrics   :: Metrics
    -- , configEkgServer :: ThreadId
    -- , configLogEnv    :: LogEnv
    , configPort :: Port
    }

getConfig :: IO Config
getConfig = do
    pool <- runNoLoggingT $ createPostgresqlPool (connStr "") 4
    return $ Config pool 8000

connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=172.19.7.103 dbname=wiki" <> sfx <> " user=wiki password=wiki port=5432"
 