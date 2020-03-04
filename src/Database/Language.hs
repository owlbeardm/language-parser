module Database.Language
  (
  ) where

import           ClassyPrelude
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Data.Time
import              Database.Base

data Language = Language
 {    langId          :: Int
    , langVersion   :: Int
    , langCreatedby :: String
    , langCreatedwhen :: LocalTime
    , langModiby    :: String
    , langModiwhen    :: LocalTime
    , langName      :: String
 }

instance Show Language where
    show lang = mconcat [ show $ langId lang
      , ".) "
      , langName lang]

instance FromRow Language where
  fromRow = Language <$> field <*> field <*> field <*> field <*> field <*> field <*> field

getLangs :: Connection -> IO [Language]
getLangs = getByQuery "SELECT id,version,createdby,modiby,name FROM language_tbl" ()

getLangByName :: String -> Connection -> IO [Language]
getLangByName lName = getByQuery qry (Only lName)
  where
    qry = "SELECT id,version,createdby,modiby,name \
          \FROM language_tbl AS l WHERE l.name = (?) \
          \LIMIT 1"

getLangIdByName :: String -> Connection -> IO (Maybe Int)
getLangIdByName lName conn = do
  results <- getLangByName lName conn
  case results of
    [lang] -> return $ Just (langId lang)
    _ -> return Nothing