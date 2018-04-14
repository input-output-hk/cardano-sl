import           Universum
import           Spec (spec)
import           Test.Hspec (hspec)
import qualified Test.Pos.Binary.Core.Txp as Txp

main :: IO ()
main = do
    hspec spec
    Txp.main
