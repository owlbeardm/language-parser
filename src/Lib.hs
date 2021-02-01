module Lib
  (
  main
  ) where

import           ClassyPrelude     (IO, print, show, ($))
import           Control.Exception (finally)
import           HTTP.API          (runServer)
import           HTTP.Config
import           Say               (say)

-- main :: IO ()
-- main = runServer

main :: IO ()
main = do
  say "in runAppDevel"
  config <- getConfig
  print $ show config
  runServer config
    `finally` say "server is closed"
