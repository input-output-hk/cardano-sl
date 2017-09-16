{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GenesisDelegation
       ( HasGenesisDelegation
       , withGenesisDelegation
       , genesisDelegation
       ) where

import           Data.Reflection        (Given (..), give)
import           Pos.Core.Genesis.Types (GenesisDelegation)

type HasGenesisDelegation = Given GenesisDelegation

withGenesisDelegation :: GenesisDelegation -> (HasGenesisDelegation => r) -> r
withGenesisDelegation = give

genesisDelegation :: HasGenesisDelegation => GenesisDelegation
genesisDelegation = given
