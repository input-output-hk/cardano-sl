-- | Arbitrary instances for Update System types

module Pos.Arbitrary.Update.MemState
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..))

import           Pos.Arbitrary.Update.Core ()
import           Pos.Binary.Class (biSize)
import           Pos.Core.Configuration (HasConfiguration)
import qualified Pos.Update.MemState as Upd

import           Test.Pos.Arbitrary.Crypto ()

instance HasConfiguration => Arbitrary Upd.MemPool where
    arbitrary = do
        proposals <- arbitrary
        votes <- arbitrary
        return $ Upd.MemPool proposals votes (biSize proposals + biSize votes)
