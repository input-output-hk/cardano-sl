{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' instances for types in 'cardano-sl-infra'

module Test.Pos.Infra.Arbitrary () where

import           Universum

import qualified Data.ByteString as BS
import           Network.Kademlia.HashNodeId (HashId (..))
import           Test.QuickCheck (Arbitrary (..), choose, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Chain.Delegation (ProxySKHeavy)
import           Pos.Infra.Communication.Types.Protocol (HandlerSpec (..),
                     VerInfo (..))
import           Pos.Infra.Communication.Types.Relay (DataMsg (..), InvMsg (..),
                     MempoolMsg (..), ReqMsg (..))
import           Pos.Infra.DHT (DHTData (..), DHTKey (..))

import           Test.Pos.Chain.Delegation.Arbitrary ()
import           Test.Pos.Chain.Update.Arbitrary ()

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

instance Arbitrary VerInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg ProxySKHeavy) where
    arbitrary = genericArbitrary
    shrink = genericShrink
