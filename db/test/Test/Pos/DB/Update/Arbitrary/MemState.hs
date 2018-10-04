{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Update System types

module Test.Pos.DB.Update.Arbitrary.MemState
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..))

import           Pos.Binary.Class (biSize)
import qualified Pos.DB.Update as Upd

import           Test.Pos.Chain.Update.Arbitrary ()
import           Test.Pos.Crypto.Arbitrary ()

instance Arbitrary Upd.MemPool where
    arbitrary = do
        proposals <- arbitrary
        votes <- arbitrary
        return $ Upd.MemPool proposals votes (biSize proposals + biSize votes)
