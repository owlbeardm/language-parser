module Main where

import           Lib
import qualified PhoneticChanges  as PC
import           System.Directory
import           System.IO

-- main :: IO ()
-- main = do
--     -- userInput <- getContents
--     createDirectory "userInput"
--     putStrLn "done!"

main :: IO ()
main = do
--  myFile <- openFile "langs/proto material/initial_words" ReadMode
 fileName <- getLine
 putStrLn (fileName ++ " words list")
 input <- readFile ("langs/" ++ fileName ++ "/initial_words")
 putStrLn (foldr (++) "." ((map (++ ", ") . lines) input))
