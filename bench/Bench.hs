import           ClassyPrelude
import           Criterion.Main
import           Database.Base
import           Database.Language
import           Print.Console

isPrime :: Integer -> Bool
isPrime n = all notDividedBy [2 .. n-1]
  where
    notDividedBy m = n `mod` m /= 0


isPrimeU :: Integer -> Bool
isPrimeU n = go 2
  where
    go x = case x > n-1 of
             True -> True
             False -> case n `mod` x /= 0 of
                        True  -> go (x+1)
                        False -> False

primeNumber :: Integer
primeNumber = 16183

main :: IO ()
main = defaultMain [
    bench "listLangs" $ whnf runSQLAction listLangs,
    bench "printWordsFrom Titan" $ whnf printWordsFrom Titan,
    bench "printTranslate wild" $ whnf printTranslate "wild",
    -- bench "printTranslate wild" $ whnf cEvolveAllLangWithAll ()
    -- bench "isVowel11 (declarative)" $ whnf isPrimeU primeNumber
  ]
