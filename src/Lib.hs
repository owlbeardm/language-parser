module Lib
  (
  main
  ) where


import           ClassyPrelude
import           Control.Monad.Logger
import           Database.Base
import           Database.Esqueleto
import           Database.Language
import           Database.Translation
import           Database.Word
import           Print.Console

-- main :: IO ()
-- main = runSQLAction $ printMigration migrateAll

main :: IO ()
main = print "Hello"

test :: IO ()
test = runSQLAction $ do
  l <- returnLang 1
  mapM_ (print . tshow . entityVal) l