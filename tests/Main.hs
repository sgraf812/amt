import qualified Data.AMT.Properties
import qualified Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  props <- testSpec "properties" (parallel Data.AMT.Properties.spec)
  Test.Tasty.defaultMain $ Test.Tasty.testGroup "amt"
    [ props
    ]
