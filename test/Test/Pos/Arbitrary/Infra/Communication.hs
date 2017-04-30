{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Arbitrary.Infra.Communication () where

import           Data.DeriveTH                    (derive, makeArbitrary)
import           Node.Message                     (MessageName (..))
import           Test.QuickCheck                  (Arbitrary (..), choose, oneof)
import           Universum

import           Pos.Communication.Types.Protocol (HandlerSpec (..), VerInfo (..))
import           Pos.Communication.Types.Relay    (InvMsg (..), MempoolMsg (..),
                                                   ReqMsg (..))
import           Pos.Types.Arbitrary              ()

instance (Arbitrary key, Arbitrary tag) => Arbitrary (ReqMsg key tag) where
    arbitrary = ReqMsg <$> arbitrary <*> arbitrary

instance (Arbitrary tag) => Arbitrary (MempoolMsg tag) where
    arbitrary = MempoolMsg <$> arbitrary

instance (Arbitrary key, Arbitrary tag) => Arbitrary (InvMsg key tag) where
    arbitrary = InvMsg <$> arbitrary <*> arbitrary

instance Arbitrary HandlerSpec where
    arbitrary = oneof
        [ ConvHandler <$> arbitrary
        , pure OneMsgHandler
        , UnknownHandler <$> choose (128, 255) <*> arbitrary
        ]

derive makeArbitrary ''MessageName
derive makeArbitrary ''VerInfo
