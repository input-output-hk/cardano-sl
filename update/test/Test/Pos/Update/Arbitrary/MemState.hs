{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Update System types

module Test.Pos.Update.Arbitrary.MemState
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..))

import           Pos.Binary.Class (biSize)
import qualified Pos.Update.MemState as Upd

import           Test.Pos.Crypto.Arbitrary ()
import           Test.Pos.Update.Arbitrary.Core ()

instance Arbitrary Upd.MemPool where
    arbitrary = do
        proposals <- arbitrary
        votes <- arbitrary
        return $ Upd.MemPool proposals votes (biSize proposals + biSize votes)
