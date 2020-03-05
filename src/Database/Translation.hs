module Database.Translation
  ( getTranslations,
  getTranslationsByWord,
  getFullTranslation,
  FullTranslation,
  Translation
  ) where

import           ClassyPrelude hiding (Word)
import           Data.Time
import           Database.Base
import           Database.Word
import           Database.Language
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

data Translation = Translation
 {   translationId         :: Int
    , version   :: Int
    , from_word_id      :: Int
    , to_lang_id      :: Int
    , to_word_id      :: Maybe Int
    , comment      :: Maybe Text
    , alt_translation      :: Maybe Text
 }

data FullTranslation = FullTranslation
 {   translation         :: Translation
    , fromWord   :: Word
    , toLang      :: Language
    , toWord      :: Maybe Word
 }


instance Show Translation where
    show translation = mconcat [ show $ translationId translation
      , ".) "
      , show $ to_lang_id translation
      , " "
      , scomment
      , " "
      , altTranslation]
      where 
        altTranslation = case alt_translation translation of
          Just a -> show a
          _ -> ""
        scomment = case comment translation of
          Just a -> show a
          _ -> ""

instance FromRow Translation where
  fromRow = Translation <$> field <*> field <*> field <*> field <*> field <*> field <*> field


getTranslations :: Connection -> IO [Translation]
getTranslations = getByQuery "SELECT id,version,from_word_id,to_lang_id,to_word_id,comment,alt_translation FROM translations_tbl" ()

getTranslationsByWordId :: ID -> Connection -> IO [Translation]
getTranslationsByWordId transId = getByQuery qry (Only $ pkId transId)
  where
    qry = "SELECT id,version,from_word_id,to_lang_id,to_word_id,comment,alt_translation \
          \FROM translations_tbl WHERE translations_tbl.from_word_id = (?) "

getTranslationsByWord :: String -> Connection -> IO [Translation]
getTranslationsByWord word conn = do
  wordIds <- getWordIdsByWord word conn
  foldr transferIO (return []) 
    $ map (`getTranslationsByWordId` conn) wordIds

transferIO :: IO [Translation] -> IO [Translation] -> IO [Translation]
transferIO a b =  do 
 la <- a
 lb <- b
 return $ la ++ lb

getFullTranslation :: Translation -> Connection -> IO (Maybe FullTranslation)
getFullTranslation trans conn = do 
  fW <- getWordById (createId $ from_word_id trans) conn
  tLan <- getLangById (createId $ to_lang_id trans) conn 
  tW <- mapM (\wId -> getWordById  (createId $ wId) conn) $ to_word_id trans
  return $ maybeFullTranslation trans fW tW tLan

maybeFullTranslation :: Translation -> Maybe Word -> Maybe (Maybe Word) -> Maybe Language -> Maybe FullTranslation
maybeFullTranslation tr fw tw tl = do
  fWd <- fw
  tWd <- tw
  tLn <- tl
  return $ FullTranslation {translation = tr, fromWord = fWd, toLang = tLn, toWord = tWd }