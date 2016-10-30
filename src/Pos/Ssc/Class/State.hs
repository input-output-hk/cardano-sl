{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.Class.State
       ( SscState(..)
       ) where

import           Data.Tagged         (Tagged (..))
import           Universum

import           Pos.Ssc.Class.Types (SscTypes (..))
import           Pos.Types.Types     (FtsSeed, SlotId)

class SscTypes a => SscState a where
    -- TODO: rename FtsSeed to SharedSeed
    calculateSeed :: Tagged a (SscInternalState a ->
                               Either (SscSeedError a) FtsSeed)

