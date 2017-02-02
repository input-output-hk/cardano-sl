{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Communication types.

module Pos.Communication.Arbitrary () where

import           Data.DeriveTH                    (derive, makeArbitrary)
import           Node.Message                     (MessageName (..))
import           Test.QuickCheck                  (Arbitrary (..), choose, oneof)
import           Universum

import           Pos.Communication.Types          (HandlerSpec (..), VerInfo (..))
import           Pos.Communication.Types.SysStart (SysStartRequest (..),
                                                   SysStartResponse (..))

derive makeArbitrary ''SysStartRequest
derive makeArbitrary ''SysStartResponse
derive makeArbitrary ''MessageName

instance Arbitrary HandlerSpec where
    arbitrary = oneof
        [ ConvHandler <$> arbitrary
        , pure OneMsgHandler
        , UnknownHandler <$> choose (2, 255) <*> arbitrary
        ]

derive makeArbitrary ''VerInfo
