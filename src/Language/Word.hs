module Language.Word where

import           ClassyPrelude      hiding (Word, delete, groupBy, isNothing,
                                     on, (\\))
import           Database.Entity
import           Database.Esqueleto
import           Language.Sounds

getWordsSounds :: [Entity Word] -> Text
getWordsSounds = pack . remove . unpack . sort . mconcat . map (wordWord . entityVal)
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)

getWordsConstClusters :: [Entity Word] -> [Text]
getWordsConstClusters = getConsonantClustersFiltered filterAllClusters . map (wordWord . entityVal)

getWordsConstStartingClusters :: [Entity Word] -> [Text]
getWordsConstStartingClusters = getConsonantClustersFiltered filterFirstCluster . map (wordWord . entityVal)

getWordsConstLastClusters :: [Entity Word] -> [Text]
getWordsConstLastClusters = getConsonantClustersFiltered filterLastCluster . map (wordWord . entityVal)
