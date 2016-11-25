{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Types for Shared Seed calculation.

module Pos.Ssc.Class.Types
       ( Ssc(..)
       ) where

import           Data.Binary         (Binary)
import           Data.SafeCopy       (SafeCopy)
import           Data.Tagged         (Tagged)
import           Data.Text.Buildable (Buildable)
import           Universum

-- | Main Shared Seed Calculation type class. Stores all needed type
-- parameters for general implementation of SSC.
class (Typeable ssc
      ,Typeable (SscPayload ssc)
      ,Typeable (SscGlobalState ssc)
      ,Typeable (SscStorage ssc)
      ,Typeable (SscProof ssc)
      ,Typeable (SscSeedError ssc)
      ,Eq (SscProof ssc)
      ,Show (SscProof ssc)
      ,Show (SscPayload ssc)
      ,Buildable (SscPayload ssc)
      ,Buildable (SscSeedError ssc)
      ,Binary (SscProof ssc)
      ,Binary (SscPayload ssc)
      ,SafeCopy (SscProof ssc)
      ,SafeCopy (SscGlobalState ssc)
      ,SafeCopy (SscPayload ssc)
      ,SafeCopy (SscStorage ssc)) =>
      Ssc ssc where

    -- | Internal SSC state stored on disk
    type SscStorage ssc
    -- | Internal SSC state stored in memory
    type SscLocalData ssc
    -- | Payload which will be stored in main blocks
    type SscPayload ssc
    -- | Global state, which formed from all known blocks
    type SscGlobalState ssc
    -- | Proof that SSC payload is correct (it'll be included into block
    -- header)
    type SscProof ssc
    -- | Error that can happen when calculating the seed
    type SscSeedError ssc

    -- | Create proof (for inclusion into block header) from payload
    mkSscProof :: Tagged ssc (SscPayload ssc -> SscProof ssc)

    -- | Remove from all data, which can make global state inconsistent
    sscFilterPayload :: SscPayload ssc -> SscGlobalState ssc -> SscPayload ssc
