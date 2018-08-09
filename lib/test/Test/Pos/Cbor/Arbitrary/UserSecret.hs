{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Cbor.Arbitrary.UserSecret
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Util.UserSecret (UserSecret, WalletUserSecret)
import           System.FileLock (FileLock)

instance Arbitrary WalletUserSecret where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Maybe FileLock) => Arbitrary UserSecret where
    arbitrary = genericArbitrary
    shrink = genericShrink
