module Database.Base
  (getByQuery,
  enumFromField,
  ID,
  pkId,
  getMaybeOne,
  createId
  ) where

import           ClassyPrelude
import           Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField        ( FieldParser, returnError, ResultError (..), typename )
import qualified Data.ByteString as BS
import           Database.PostgreSQL.Simple.FromRow

data ID = ID
  {   pkId         :: Int
  }

createId a = ID {pkId = a}

instance FromRow ID where
  fromRow = ID <$> field


getByQuery :: (FromRow r, ToRow q) => Query -> q -> Connection -> IO [r]
getByQuery qry params conn = query conn qry params

getMaybeOne :: IO [a] -> IO (Maybe a)
getMaybeOne list = do
  l <- list
  case l of 
    [] -> return Nothing
    (x:xs) -> return (Just x)
    _ -> return Nothing



enumFromField :: (Typeable a) => (BS.ByteString -> Maybe a) -> FieldParser a
enumFromField enumParser field mdata = do
    n <- typename field
    if n /= "text"
    then returnError Incompatible field ""
    else case mdata of
        Nothing -> returnError UnexpectedNull field ""
        Just bs -> case enumParser bs of
            Nothing -> returnError ConversionFailed field (show bs)
            Just x  -> return x