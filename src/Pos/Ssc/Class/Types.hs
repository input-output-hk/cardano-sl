{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Types for Shared Seed calculation.

module Pos.Ssc.Class.Types
       ( Ssc(..)
       ) where

import           Data.Default        (Default)
import           Data.Tagged         (Tagged)
import           Data.Text.Buildable (Buildable)
import           Universum

import           Pos.Binary.Class    (Bi)

-- | Main Shared Seed Calculation type class. Stores all needed type
-- parameters for general implementation of SSC.
class (Typeable ssc
      ,Typeable (SscPayload ssc)
      ,Typeable (SscProof ssc)
      ,Typeable (SscSeedError ssc)
      ,Eq (SscProof ssc)
      ,Show (SscProof ssc)
      ,Show (SscPayload ssc)
      ,Buildable (SscPayload ssc)
      ,Buildable (SscSeedError ssc)
      ,Bi (SscProof ssc)
      ,Bi (SscPayload ssc)
      ,Default (SscLocalData ssc)
      ) =>
      Ssc ssc where

    -- | Internal SSC state stored in memory
    type SscLocalData ssc
    -- | Payload which will be stored in main blocks
    type SscPayload ssc
    -- | Global state, which is formed from all known blocks
    type SscGlobalState ssc
    -- | Proof that SSC payload is correct (it'll be included into block
    -- header)
    type SscProof ssc
    -- | Error that can happen when calculating the seed
    type SscSeedError ssc
    -- | SSC specific context in NodeContext
    type SscNodeContext ssc
    -- | Needed options for creating SscNodeContext
    type SscParams ssc

    -- | Create proof (for inclusion into block header) from payload
    mkSscProof :: Tagged ssc (SscPayload ssc -> SscProof ssc)

    -- | Create SscNodeContext
    sscCreateNodeContext :: MonadIO m => SscParams ssc -> m (SscNodeContext ssc)
