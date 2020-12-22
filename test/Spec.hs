import           ClassyPrelude
import           Language.SoundsSpec
import           HTTP.APISpec
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
             [ soundsSpecs,
               httpSpecs
             ]
--   goldens <- goldenTests
  defaultMain (testGroup " All Tests" [
                  testGroup "Specs" specs
                -- , testGroup "Properties" props
                -- , testGroup "Golden Tests" goldens
                ])
  -- hspec httpSpecs
-- main = defaultMain (testGroup "(no tests)" [])
