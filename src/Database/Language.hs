module Database.Language
  ( getLangIdByName,
    getLangs,
    Language,
    getLangById
  ) where

import           ClassyPrelude
import           Data.Time
import           Database.Base
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import qualified Data.ByteString as BS


data LanguageName = ProtoMaterial
    | ProtoMonster
    | English 
    deriving (Eq,Enum,Show)  

parseLanguageName :: BS.ByteString -> Maybe LanguageName
parseLanguageName bs = case decodeUtf8 bs of
    "ProtoMaterial" -> Just ProtoMaterial
    "ProtoMonster" -> Just ProtoMonster
    "English" -> Just English
    _ -> Nothing

instance FromField LanguageName where
   fromField = enumFromField parseLanguageName


data Language = Language
 {    langId          :: Int
    , langVersion     :: Int
    , langCreatedby   :: String
    , langCreatedwhen :: LocalTime
    , langModiby      :: String
    , langModiwhen    :: LocalTime
    , langName        :: LanguageName
 }

instance Show Language where
    show lang = mconcat [ show $ langId lang
      , ".) "
      , show $ langName lang]

instance FromRow Language where
  fromRow = Language <$> field <*> field <*> field <*> field <*> field <*> field <*> field

getLangs :: Connection -> IO [Language]
getLangs = getByQuery "SELECT id,version,createdby,createdwhen,modiby,modiwhen,name FROM language_tbl" ()

getLangByName :: String -> Connection -> IO [Language]
getLangByName lName = getByQuery qry (Only lName)
  where
    qry = "SELECT id,version,createdby,createdwhen,modiby,modiwhen,name \
          \FROM language_tbl AS l WHERE l.name = (?) \
          \LIMIT 1"

getLangById :: ID -> Connection -> IO (Maybe Language)
getLangById lId conn = getMaybeOne $ getByQuery qry (Only $ pkId lId) conn
  where
    qry = "SELECT id,version,createdby,createdwhen,modiby,modiwhen,name \
          \FROM language_tbl AS l WHERE l.id = (?) \
          \LIMIT 1"

getLangIdByName :: String -> Connection -> IO (Maybe Int)
getLangIdByName lName conn = do
  results <- getLangByName lName conn
  case results of
    [lang] -> return $ Just (langId lang)
    _      -> return Nothing
