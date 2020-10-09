module Test.Test where

import           ClassyPrelude
import           Text.Regex.Base
import           Text.Regex.PCRE
-- ("qu","u","itquuit",[])
replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = ""
replaceAll rexp target source  =
    let res :: (String,String,String,[String])
        res = (source =~ rexp)
        rpAll = replaceAll rexp target
    in first4 res ++ target ++ rpAll (third4 res)
    where
        first4 (a,_,_,_) = a
        third4 (_,_,c,_) = c

main :: IO ()
main = let a :: String
           a = "quuitquuit"
           ad :: String
           ad = "u(?=i)"
        --    b :: Bool
        --    b = (a =~ ad)
        --    c :: [MatchArray]
        --    c = (a =~ ad)
        --    d :: (String,String,String,[String])
        --    d = (a =~ ad)
       in print (replaceAll ad "a" a)


