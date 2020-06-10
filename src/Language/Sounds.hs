module Language.Sounds where

import           ClassyPrelude
-- import           Data.List
import qualified          Data.Text      as T

vovels :: Text
vovels = "iyɨʉɯuɪʏɪ̈ʊ̈ʊeøɘɵɤoəɛœɜɞʌɔæɐaɶäɒ̈ɑɒūúāáīíḗ"


isVowel :: Char -> Bool
isVowel v = isJust $ T.findIndex (== v) vovels

splitOnAnyOf :: [Text] -> Text -> [Text]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= T.splitOn d) [xs] ds

getConsonantClusters :: [Text] -> [Text]
getConsonantClusters = remove . (sortBy clSort) . mconcat . map (splitOnAnyOf (T.group vovels))
  where 
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)
    clSort a b
      | length a > length b = GT
      | length a < length b = LT
      | otherwise = compare a b


-- Ordering = LT | EQ | GT



