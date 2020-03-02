{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( main
    ) where

import           ClassyPrelude

import           Data.Monoid       (mconcat)
import           Data.Typeable
import qualified Platform.HTTP     as HTTP
import qualified Platform.Postgres as PG
import           Web.Scotty

-- main = scotty 3000 $
  -- get "/words/:lang" $ do
    -- beam <- param "lang"
    -- html $ mconcat ["<h1>words , ", beam, " me up!</h1>"]

main :: IO ()
main = do
  pgEnv <- PG.init
  let runner app = flip runReaderT pgEnv $ unAppT app
  HTTP.main runner

type Env = PG.Env

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  } deriving  ( Applicative, Functor, Monad
              , MonadIO, MonadReader Env)

