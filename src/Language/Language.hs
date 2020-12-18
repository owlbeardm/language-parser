module Language.Language
  (
  evolveWordText   
  ) where

import           ClassyPrelude   hiding (Word, keys, on, words)
import qualified Data.Text       as T
import           Database.Entity
import qualified Text.Regex      as R


evolveWordText :: WordText -> [EvolveLaw] -> WordText
evolveWordText = foldl' changeWord

-- |The 'changeWord' function changes a word.
changeWord :: WordText -> EvolveLaw -> WordText
changeWord wordText law = T.pack $ R.subRegex regex word soundTo
   where
      regex = (R.mkRegex . T.unpack . evolveLawSoundRegexFrom) law
      soundTo = (T.unpack . evolveLawSoundTo) law
      word = T.unpack wordText