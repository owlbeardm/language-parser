{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( 
  ) where

import           ClassyPrelude
-- import           Control.Applicative
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

mainPrint :: (Show a) => (Connection -> IO [a]) -> IO ()
mainPrint action = do
  pool <- acquirePool
  a <- withConn pool action
  mapM_ print a

data Language = Language
 {  langId            :: Int
    , langVersion     :: Int
    , langCreatedby   :: String
    -- , langCreatedwhen :: Day
    , langModiby      :: String
    -- , langModiwhen    :: Day
    , langName        :: String
 }

instance Show Language where
    show lang = mconcat [ show $ langId lang
      , ".) "
      , langName lang]

instance FromRow Language where
  fromRow = Language <$> field <*> field <*> field <*> field <*> field

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

getLangs :: Connection -> IO [Language]
getLangs = getByQuery "SELECT id,version,createdby,modiby,name FROM language_tbl" ()

getLangByName :: String -> Connection -> IO [Language]
getLangByName langName = getByQuery qry (Only langName)
  where
    qry = "SELECT id,version,createdby,modiby,name \
          \FROM language_tbl AS l WHERE l.name = (?) "

acquirePool :: IO (Pool Connection)
acquirePool = do
  createPool (connect connInfo) close 1 10 10
  where
    connInfo = defaultConnectInfo {connectHost = "172.20.7.103", connectUser = "wiki", connectPassword = "wiki", connectDatabase = "wiki"}

withConn :: Pool Connection -> (Connection -> IO a) -> IO a
withConn pool action = withResource pool (\conn -> action conn)


--   getAllWords :: Connection -> IO [LangWord]
-- getAllWords pool = do
--   a <- withResource pool (\conn -> query_ conn "SELECT id,version,createdby,modiby,word FROM word_tbl" :: IO [LangWord])
--   mapM_ print a