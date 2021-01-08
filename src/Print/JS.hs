module Print.JS where

import           ClassyPrelude        (IO, ($))
import           Data.Aeson           (encode)
import           Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import           Data.Proxy           (Proxy (..))
import           HTTP.LanguageAPI     (LangsApi)
import           Servant.Swagger      (toSwagger)

main :: IO ()
main = BSL.putStrLn $ encode $ toSwagger (Proxy :: Proxy LangsApi)
