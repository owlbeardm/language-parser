module Lib
  ( main,
  mainPrintL,
  mainPrintML,
  mainPrintOne,
  printTranslation
  ) where

import           ClassyPrelude
-- import           Control.Applicative
import           Data.Pool
import           Database.Base
import           Database.Language
import           Database.Word
import           Database.Translation
import           Database.PostgreSQL.Simple

main :: IO ()
main = putStr "hello"

mainPrintL :: (Show a) => (Connection -> IO [a]) -> IO ()
mainPrintL action = do
  pool <- acquirePool
  a <- withResource pool action
  mapM_ print a

mainPrintML :: (Show a) => (Connection -> IO (Maybe [a])) -> IO ()
mainPrintML action = do
  pool <- acquirePool
  a <- withResource pool action
  case a of
    Just list -> mapM_ print list
    _         -> print a

mainPrintOne :: (Show a) => (Connection -> IO a) -> IO ()
mainPrintOne action = do
  pool <- acquirePool
  a <- withResource pool action
  print a


acquirePool :: IO (Pool Connection)
acquirePool = createPool (connect connInfo) close 1 10 10
  where
    connInfo = defaultConnectInfo {connectHost = "172.20.7.103", connectUser = "wiki", connectPassword = "wiki", connectDatabase = "wiki"}

withConn :: Pool Connection -> (Connection -> IO a) -> IO a
withConn  = withResource

printTranslation :: String -> IO ()
printTranslation word = do 
  pool <- acquirePool
  translations <- withResource pool $ getTranslationsByWord word
  mapM_ print $ mapM (print . liftIO )  $ fmap (getFullTranslationByTranslation pool) translations

getFullTranslationByTranslation :: Pool Connection -> Translation ->  IO (Maybe FullTranslation)
getFullTranslationByTranslation pool trans = withResource pool $ getFullTranslation trans