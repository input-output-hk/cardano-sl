module Test.Pos.Arbitrary.Infra.DHT () where

import qualified Data.ByteString             as BS (pack)
import           Network.Kademlia.HashNodeId (HashId (..))
import           Universum

import           Pos.DHT                     (DHTData (..), DHTKey (..))
import           Test.QuickCheck             (Arbitrary (..))

deriving instance Arbitrary DHTData

instance Arbitrary DHTKey where
    arbitrary = DHTKey . HashId . BS.pack <$> arbitrary
