module Lib
  (
  main
  ) where

import           ClassyPrelude (IO)
import           Platform.HTTP (runServer)

main :: IO ()
main = runServer
