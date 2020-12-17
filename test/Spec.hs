import           ClassyPrelude
import           Language.SoundsSpec
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
             [ soundsSpecs
            --  , lookupIPSpecs
             ]
--   goldens <- goldenTests
  defaultMain (testGroup " All Tests" [
                  testGroup "Specs" specs
                -- , testGroup "Properties" props
                -- , testGroup "Golden Tests" goldens
                ])
-- main = defaultMain (testGroup "(no tests)" [])
