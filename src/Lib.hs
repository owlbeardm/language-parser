module Lib
  (
  main
  ) where


import           ClassyPrelude
import           Database.Base
import           Database.Esqueleto
import           Database.Language

-- main :: IO ()
-- main = runSQLAction $ printMigration migrateAll

-- main :: IO ()
-- main = print "Hello"

main :: IO ()
main = runSQLAction $ do
  l <- findLangById 1
  mapM_ (print . tshow . entityVal) l
