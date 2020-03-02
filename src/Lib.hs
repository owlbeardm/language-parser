module Lib
  ( libMain
  ) where

import           Control.Applicative
import           Data.Time
-- import           Database.PostgreSQL.Simple
-- import           Database.PostgreSQL.Simple.FromRow

libMain :: IO ()
libMain = print ['a', 'b']

data Language = Language
 {  langId            :: Int
    , langVersion     :: Int
    , langCreatedby   :: String
    , langCreatedwhen :: Day
    , langModiby      :: String
    , langModiwhen    :: Day
    , langName        :: String
 }

instance Show Language where
    show lang = mconcat [ show $ langId lang
      , ".) "
      , langName lang]

data LangWord = LangWord
 {   wordId           :: Int
    , wordVersion     :: Int
    , wordCreatedby   :: String
    , wordCreatedwhen :: Day
    , wordModiby      :: String
    , wordModiwhen    :: Day
    , wordText        :: String
 }

instance Show LangWord where
    show word = mconcat [ show $ wordId word
      , ".) "
      , wordText word]

addWord :: String -> IO ()
addWord word = do
 conn <- open "tools.db"
 execute conn "INSERT INTO word_tbl (word) VALUES (?)"
 (Only word)
 print "word added"
 close conn

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
 conn <- open dbName
 action conn
 close conn
