import qualified Migrations
import           Test.Tasty
import           Universum

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests"
  [ Migrations.tests ]
