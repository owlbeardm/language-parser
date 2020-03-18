module Print.Console where

import           ClassyPrelude hiding (words)
import           Database.Base
import           Database.Language
import           Database.Word
import           Database.Esqueleto

printLangs :: IO ()
printLangs = runSQLAction $ do
     langs <- listLangs
     liftIO $ mapM_ (putStrLn . tshow . entityVal) langs

printWordsFrom :: LanguageName -> IO ()
printWordsFrom langName = runSQLAction $ do
     words <- listWordsByLang langName
     liftIO $ mapM_ (putStrLn . showWord . entityVal) words
     putStr "\n\tTotal: "
     print $ length words