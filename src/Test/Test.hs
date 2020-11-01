module Test.Test where

import           ClassyPrelude
import           Text.RE.REOptions
import           Text.Regex.Base
import           Text.Regex.PCRE
-- ("qu","u","itquuit",[])
replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = ""
replaceAll rexp target source  =
    let res :: (String,String,String,[String])
        res = (source =~ rexp)
        rpAll = replaceAll rexp target
    in if null (second4 res)
        then source
        else first4 res ++ target ++ rpAll (third4 res)
    where
        first4 (a,_,_,_) = a
        second4 (_,b,_,_) = b
        third4 (_,_,c,_) = c

main :: IO ()
main = let a :: String
           a = "βlaA"
           ad :: Regex String
        --    add :: String
        --    add = "A"
           ad = makeRegexOpts (defaultCompOpt - compNewline) defaultExecOpt add
        --    b :: Bool
        --    b = (a =~ ad)
        --    c :: [MatchArray]
        --    c = (a =~ ad)
           d :: (String,String,String,[String])
           d = (a =~ ad)
       in print d


