{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System types

module Pos.Update.Arbitrary.MemState
       (
       ) where

import           Pos.Crypto.Arbitrary      ()
import           Pos.Update.Arbitrary.Core ()
import qualified Pos.Update.MemState       as Upd

import           Test.QuickCheck           (Arbitrary (..))
import           Universum

instance Arbitrary Upd.MemPool where
    arbitrary = Upd.MemPool <$> arbitrary <*> arbitrary
