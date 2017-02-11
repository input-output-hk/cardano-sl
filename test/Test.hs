import           Test.NodeSpec (spec)
import           Test.Hspec (hspec)
import           Test.Util (makeTCPTransport, makeInMemoryTransport)

main :: IO ()
main = hspec spec
