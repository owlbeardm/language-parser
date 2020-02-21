module Main where

import           Lib
import qualified PhoneticChanges  as PC
import           System.Directory

main :: IO ()
main = do
    -- userInput <- getContents
    createDirectory "userInput"
    putStrLn "done!"