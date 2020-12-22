module Lib
  (
  main
  ) where

import           ClassyPrelude (IO)
import           HTTP.API (runServer)

main :: IO ()
main = runServer
