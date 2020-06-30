module Language.Sounds where

import           ClassyPrelude
-- import           Data.List
import qualified          Data.Text      as T

vovels :: Text
vovels = "iyɨʉɯuɪʏɪ̈ʊ̈ʊeøɘɵɤoəɛœɜɞʌɔæɐaɶäɒ̈ɑɒūúāáīíḗee̯ː"


isVowel :: Char -> Bool
isVowel v = isJust $ T.findIndex (== v) vovels

splitOnAnyOf :: [Text] -> Text -> [Text]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= T.splitOn d) [xs] ds

getConsonantClustersFiltered :: (Text -> [Text] -> [Text]) -> [Text] -> [Text]
getConsonantClustersFiltered fltr = remove . sortBy clSort . mconcat . map (splitAndFilterWord fltr)
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
    splitAndFilterWord f w = (f w . splitOnAnyOf (T.group vovels)) w

filterFirstCluster :: Text -> [Text] -> [Text]
filterFirstCluster wrd lst 
  | null wrd = []
  | take 1 wrd `isInfixOf` vovels = []
  | otherwise = [(head . impureNonNull) lst | (not . null) lst]

filterLastCluster :: Text -> [Text] -> [Text]
filterLastCluster wrd lst 
  | null wrd = []
  | T.takeEnd 1 wrd `isInfixOf` vovels = []
  | otherwise = [(head . impureNonNull . reverse) lst | (not . null) lst]

filterAllClusters :: Text -> [Text] -> [Text]
filterAllClusters _ lst  = lst