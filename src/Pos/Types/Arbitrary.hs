{-# LANGUAGE StandaloneDeriving #-}

-- | Arbitrary instances.

module Pos.Types.Arbitrary
       (
       ) where

import qualified Data.ByteString           as BS
import           Test.QuickCheck           (Arbitrary (..), vector)
import           Test.QuickCheck.Instances ()
import           Universum

import           Pos.Types.FtsSeed         (ftsSeedLength)
import           Pos.Types.Types           (Coin (Coin))

deriving instance Arbitrary Coin
