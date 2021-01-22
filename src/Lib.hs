module Lib
  (
  main
  ) where

import           ClassyPrelude     (IO)
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
  runServer config
    `finally` say "server is closed"
