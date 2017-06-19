{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System types

module Pos.Update.Arbitrary.MemState
       (
       ) where

import           Universum

import           Test.QuickCheck           (Arbitrary (..))

import           Pos.Binary.Class          (biSize)
import           Pos.Crypto.Arbitrary      ()
import           Pos.Update.Arbitrary.Core ()
import qualified Pos.Update.MemState       as Upd

instance Arbitrary Upd.MemPool where
    arbitrary = do
        proposals <- arbitrary
        votes <- arbitrary
        return $ Upd.MemPool proposals votes (biSize proposals + biSize votes)
