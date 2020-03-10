module Database.Word
  ( getWords,
    getWordsByLangId,
    -- getWordsByLangName,
    getWordIdsByWord,
    Word,
    getWordById
  ) where

import           ClassyPrelude hiding (Word)
import           Data.Time
import           Database.Base
import           Database.Language
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

data Word = Word
 {   wordId         :: Int
    , wordVersion   :: Int
    , wordCreatedby :: String
    , wordCreatedwhen :: LocalTime
    , wordModiby    :: String
    , wordModiwhen    :: LocalTime
    , wordText      :: String
 }

instance Show Word where
    show word = mconcat [ show $ wordId word
      , ".) "
      , wordText word]

instance FromRow Word where
  fromRow = Word <$> field <*> field <*> field <*> field <*> field <*> field <*> field


getWords :: Connection -> IO [Word]
getWords = getByQuery "SELECT id,version,createdby,createdwhen,modiby,modiwhen,word FROM word_tbl" ()

getWordsByLangId :: Int -> Connection -> IO [Word]
getWordsByLangId langId = getByQuery qry (Only langId)
  where
    qry = "SELECT id,version,createdby,createdwhen,modiby,modiwhen,word \
          \FROM word_tbl WHERE word_tbl.lang_id = (?) "

-- getWordsByLangName :: String -> Connection -> IO (Maybe [Word])
-- getWordsByLangName name conn = do
--   langId <- getLangIdByName name conn
--   mapM (`getWordsByLangId` conn) langId

getWordIdsByWord :: String -> Connection -> IO [ID]
getWordIdsByWord wText = getByQuery qry (Only wText)
  where
    qry = "SELECT id \
          \FROM word_tbl AS w WHERE w.word = (?) "

getWordById :: ID -> Connection -> IO (Maybe Word)
getWordById wId conn = getMaybeOne $ getByQuery qry (Only $ pkId wId) conn
  where
    qry = "SELECT id,version,createdby,createdwhen,modiby,modiwhen,word \
          \FROM word_tbl AS w WHERE w.id = (?) \
          \LIMIT 1"