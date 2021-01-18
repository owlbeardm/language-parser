module Print.JS where

import           ClassyPrelude              (IO, ($))
import           Data.Aeson                 (encode)
import           Data.ByteString.Lazy.Char8 as BSL (writeFile)
import           Data.Proxy                 (Proxy (..))
import           HTTP.API                   (API)
import           Servant.Swagger            (toSwagger)

main :: IO ()
main = BSL.writeFile "swagger.json" $ encode $ toSwagger (Proxy :: Proxy API)
