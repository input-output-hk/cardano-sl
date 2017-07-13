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

derive makeArbitrary ''MessageName
derive makeArbitrary ''VerInfo
