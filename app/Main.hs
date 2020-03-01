{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid      (mconcat)
import           Lib
import qualified PhoneticChanges  as PC
import           System.Directory
import           System.IO
import           Web.Scotty

-- main :: IO ()
-- main = do
--     -- userInput <- getContents
--     createDirectory "userInput"
--     putStrLn "done!"

-- main :: IO ()
-- main = do
-- --  myFile <- openFile "langs/proto material/initial_words" ReadMode
--  fileName <- getLine
--  putStrLn (fileName ++ " words list")
--  input <- readFile ("langs/" ++ fileName ++ "/initial_words")
--  putStrLn (foldr (++) "." ((map (++ ", ") . lines) input))

main = scotty 3000 $
  get "/words/:lang" $ do
    beam <- param "lang"
    html $ mconcat ["<h1>words , ", beam, " me up!</h1>"]
--   get "/:word" $ do
--     beam <- param "word"
--     html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

