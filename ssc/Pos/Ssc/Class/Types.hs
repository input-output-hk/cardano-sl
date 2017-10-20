{-# LANGUAGE TypeFamilies #-}

-- | Types for Shared Seed calculation.

module Pos.Ssc.Class.Types
       ( Ssc(..)
       , SscBlock(..)
       , HasSscContext(..)
       ) where

import           Control.Lens        (choosing, makeWrapped, _Wrapped)
import           Data.Text.Buildable (Buildable)
import           Universum

import           Pos.Binary.Class    (Bi)
import           Pos.Core            (HasDifficulty (..), HasEpochIndex (..),
                                      HasEpochOrSlot (..), HasHeaderHash (..),
                                      IsGenesisHeader, IsMainHeader)
import           Pos.Util.Util       (Some)
import           Pos.Ssc.Core.Types  (SscPayload(..), SscProof(..))

-- | Main Shared Seed Calculation type class. Stores all needed type
-- parameters for general implementation of SSC.
class ( Eq SscGlobalState
      , Buildable SscVerifyError
      , Buildable SscGlobalState
      , Bi SscProof
      , Bi SscPayload
      ) =>
      Ssc where

    -- | Internal SSC state stored in memory
    type SscLocalData
    -- | Global state, which is formed from all known blocks
    type SscGlobalState
    -- | SSC specific context in NodeContext
    type SscNodeContext
    -- | Needed options for creating SscNodeContext
    type SscParams
    -- | Type for verification error
    type SscVerifyError

    -- | Create SscNodeContext
    sscCreateNodeContext :: MonadIO m => SscParams -> m SscNodeContext

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
newtype SscBlock = SscBlock
    { getSscBlock :: Either (Some IsGenesisHeader)
                            (Some IsMainHeader, SscPayload)
    }

makeWrapped ''SscBlock

instance HasDifficulty SscBlock where
    difficultyL = _Wrapped . choosing difficultyL (_1 . difficultyL)
instance HasEpochIndex SscBlock where
    epochIndexL = _Wrapped . choosing epochIndexL (_1 . epochIndexL)
instance HasHeaderHash SscBlock where
    headerHash     = either headerHash (headerHash . fst) . getSscBlock
instance HasEpochOrSlot SscBlock where
    getEpochOrSlot = either getEpochOrSlot (getEpochOrSlot . fst) . getSscBlock

class HasSscContext ctx where
    sscContext :: Lens' ctx SscNodeContext
