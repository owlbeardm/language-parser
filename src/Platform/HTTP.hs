module Platform.HTTP
      ( 
      ) where

-- import           ClassyPrelude

-- -- import           Network.Wai        (Response)
-- import           System.Environment

-- type App r m = (MonadIO m)


-- main :: (App r m) => (m Response -> IO Response) -> IO ()
-- main runner = do
--   port <- acquirePort
--   mayTLSSetting <- acquireTLSSetting
--   scottyT port runner routes
--   where
--     acquirePort = do
--       port <- fromMaybe "" <$> lookupEnv "PORT"
--       return . fromMaybe 3000 $ readMay port
--     acquireTLSSetting = do
--       env <- (>>= readMay) <$> lookupEnv "ENABLE_HTTPS"
--       let enableHttps = fromMaybe True env
--       return $ if enableHttps
--         then Just $ tlsSettings "secrets/tls/certificate.pem" "secrets/tls/key.pem"
--         else Nothing
