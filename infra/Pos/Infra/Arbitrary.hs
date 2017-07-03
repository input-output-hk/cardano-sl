-- | 'Arbitrary' instances for types in 'cardano-sl-infra'

module Pos.Infra.Arbitrary () where

import           Universum

import qualified Data.ByteString                   as BS
import           Network.Kademlia.HashNodeId       (HashId (..))
import           Node.Message.Class                (MessageName (..))
import           Test.QuickCheck                   (Arbitrary (..), choose, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Communication.Types.Protocol  (HandlerSpec (..), VerInfo (..))
import           Pos.Communication.Types.Relay     (InvMsg (..), MempoolMsg (..),
                                                    ReqMsg (..))
import           Pos.DHT                           (DHTData (..), DHTKey (..))

deriving instance Arbitrary DHTData

instance Arbitrary DHTKey where
    arbitrary = DHTKey . HashId . BS.pack <$> arbitrary

instance (Arbitrary key) => Arbitrary (ReqMsg key) where
    arbitrary = ReqMsg <$> arbitrary

instance Arbitrary (MempoolMsg tag) where
    arbitrary = pure MempoolMsg

instance (Arbitrary key) => Arbitrary (InvMsg key) where
    arbitrary = InvMsg <$> arbitrary

instance Arbitrary HandlerSpec where
    arbitrary = oneof
        [ ConvHandler <$> arbitrary
        , UnknownHandler <$> choose (128, 255) <*> arbitrary
        ]

instance Arbitrary MessageName where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VerInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink
