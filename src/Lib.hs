{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( main
  ) where

import           ClassyPrelude
-- import           Control.Applicative
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

main :: IO ()
main = print "hello"

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
    _ -> print a

mainPrintOne :: (Show a) => (Connection -> IO a) -> IO ()
mainPrintOne action = do
  pool <- acquirePool
  a <- withResource pool action
  print a

data LangWord = LangWord
 {   wordId         :: Int
    , wordVersion   :: Int
    , wordCreatedby :: String
    -- , wordCreatedwhen :: Day
    , wordModiby    :: String
    -- , wordModiwhen    :: Day
    , wordText      :: String
 }

instance Show LangWord where
    show word = mconcat [ show $ wordId word
      , ".) "
      , wordText word]

instance FromRow LangWord where
  fromRow = LangWord <$> field <*> field <*> field <*> field <*> field

getByQuery :: (FromRow r, ToRow q) => Query -> q -> Connection -> IO [r]
getByQuery qry params conn = query conn qry params

getWords :: Connection -> IO [LangWord]
getWords = getByQuery "SELECT id,version,createdby,modiby,word FROM word_tbl" ()

getWordsByLangId :: Int -> Connection -> IO [LangWord]
getWordsByLangId langId = getByQuery qry (Only langId)
  where
    qry = "SELECT id,version,createdby,modiby,word \
          \FROM word_tbl WHERE word_tbl.lang_id = (?) "

getWordsByLangName :: String -> Connection -> IO (Maybe [LangWord])
getWordsByLangName name conn = do
  langId <- getLangIdByName name conn
  mapM (`getWordsByLangId` conn) langId

acquirePool :: IO (Pool Connection)
acquirePool = createPool (connect connInfo) close 1 10 10
  where
    connInfo = defaultConnectInfo {connectHost = "172.20.7.103", connectUser = "wiki", connectPassword = "wiki", connectDatabase = "wiki"}

withConn :: Pool Connection -> (Connection -> IO a) -> IO a
withConn  = withResource