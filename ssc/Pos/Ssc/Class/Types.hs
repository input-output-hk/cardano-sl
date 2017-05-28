{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types for Shared Seed calculation.

module Pos.Ssc.Class.Types
       ( Ssc(..)
       , SscBlock(..)
       ) where

import           Control.Lens        (choosing, makeWrapped, _Wrapped)
import           Data.Tagged         (Tagged)
import           Data.Text.Buildable (Buildable)
import           Universum

import           Pos.Binary.Class    (Bi)
import           Pos.Core            (HasDifficulty (..), HasEpochIndex (..),
                                      HasEpochOrSlot (..), HasHeaderHash (..),
                                      IsGenesisHeader, IsMainHeader)
import           Pos.Util.Util       (Some)

-- | Main Shared Seed Calculation type class. Stores all needed type
-- parameters for general implementation of SSC.
class ( Typeable ssc
      , Typeable (SscPayload ssc)
      , Typeable (SscProof ssc)
      , Typeable (SscSeedError ssc)
      , Eq (SscProof ssc)
      , Eq (SscGlobalState ssc)
      , Show (SscProof ssc)
      , Show (SscPayload ssc)
      , Buildable (SscPayload ssc)
      , Buildable (SscSeedError ssc)
      , Buildable (SscVerifyError ssc)
      , Buildable (SscGlobalState ssc)
      , Bi (SscProof ssc)
      , Bi (SscPayload ssc)
      , NFData (SscPayload ssc)
      , NFData (SscProof ssc)
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
    -- | Type for verification error
    type SscVerifyError ssc

    -- | Create proof (for inclusion into block header) from payload
    mkSscProof :: Tagged ssc (SscPayload ssc -> SscProof ssc)

    -- | Create SscNodeContext
    sscCreateNodeContext :: MonadIO m => Tagged ssc (SscParams ssc -> m (SscNodeContext ssc))

-- [CSL-1156] Find a better way for this
--
-- NB. It must be a newtype instead of a type (unlike e.g. 'TxpBlock' and
-- 'UpdateBlock'), because otherwise we can't write a 'MonadBlockDBGeneric'
-- instance for it. If you try, you'll get the following error:
--
--     Illegal type synonym family application in instance: SscBlock ssc
--
-- (Which is actually pretty fair – SscPayload isn't injective and so the
-- whole SscBlock isn't either.)
newtype SscBlock ssc = SscBlock
    { getSscBlock :: Either (Some IsGenesisHeader)
                            (Some IsMainHeader, SscPayload ssc)
    }

makeWrapped ''SscBlock

instance HasDifficulty (SscBlock ssc) where
    difficultyL = _Wrapped . choosing difficultyL (_1 . difficultyL)
instance HasEpochIndex (SscBlock ssc) where
    epochIndexL = _Wrapped . choosing epochIndexL (_1 . epochIndexL)
instance HasHeaderHash (SscBlock ssc) where
    headerHash     = either headerHash (headerHash . fst) . getSscBlock
instance HasEpochOrSlot (SscBlock ssc) where
    getEpochOrSlot = either getEpochOrSlot (getEpochOrSlot . fst) . getSscBlock
