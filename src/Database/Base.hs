module Database.Base
  (getByQuery
  ) where

import           ClassyPrelude
import           Database.PostgreSQL.Simple

getByQuery :: (FromRow r, ToRow q) => Query -> q -> Connection -> IO [r]
getByQuery qry params conn = query conn qry params