module Lib
  (
  main
  ) where


import           ClassyPrelude
import           Database.Base
import           Database.Esqueleto
import           Database.Language
import           Database.Translation
import           Database.Word

-- main :: IO ()
-- main = runSQLAction $ printMigration migrateAll

main :: IO ()
main = print "Hello"

test :: IO ()
test = runSQLAction $ do
  l <- returnLang 1
  mapM_ (print . tshow . entityVal) l

printLangs :: IO ()
printLangs = runSQLAction $ do
     langs <- listLangs
     liftIO $ mapM_ (putStrLn . tshow . entityVal) langs

printWords :: LanguageName -> IO ()
printWords langName = runSQLAction $ do
     words <- listWordsByLang langName
     liftIO $ mapM_ (putStrLn . tshow . entityVal) words
