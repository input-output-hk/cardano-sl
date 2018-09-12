module Pos.Chain.Block.IsHeader
       ( IsHeader
       , IsGenesisHeader
       , IsMainHeader (..)
       ) where

import           Control.Lens (Lens')

import           Pos.Chain.Block.HasPrevBlock (HasPrevBlock (..))
import           Pos.Chain.Block.Header (BlockHeader, GenesisBlockHeader,
                     HasHeaderHash (..), MainBlockHeader, mainHeaderLeaderKey,
                     mainHeaderSlot)
import           Pos.Chain.Update.BlockVersion (HasBlockVersion (..))
import           Pos.Chain.Update.SoftwareVersion (HasSoftwareVersion (..))
import           Pos.Core.Common (HasDifficulty (..))
import           Pos.Core.Slotting (HasEpochIndex (..), HasEpochOrSlot (..),
                     SlotId (..))
import           Pos.Crypto (PublicKey)
import           Pos.Util.Some (Some, applySome, liftLensSome)

----------------------------------------------------------------------------
-- IsHeader
----------------------------------------------------------------------------

{- | A class that lets subpackages use some fields from headers without
depending on cardano-sl:

  * 'difficultyL'
  * 'epochIndexL'
  * 'epochOrSlotG'
  * 'prevBlockL'
  * 'headerHashG'
-}
class ( HasDifficulty header
      , HasEpochIndex header
      , HasEpochOrSlot header
      , HasPrevBlock header
      , HasHeaderHash header) =>
      IsHeader header

instance HasDifficulty (Some IsHeader) where
    difficultyL = liftLensSome difficultyL

instance HasEpochIndex (Some IsHeader) where
    epochIndexL = liftLensSome epochIndexL

instance HasEpochOrSlot (Some IsHeader) where
    getEpochOrSlot = applySome getEpochOrSlot

instance HasPrevBlock (Some IsHeader) where
    prevBlockL = liftLensSome prevBlockL

instance HasHeaderHash (Some IsHeader) where
    headerHash = applySome headerHash

instance IsHeader (Some IsHeader)

instance IsHeader BlockHeader

instance IsHeader MainBlockHeader

instance IsHeader GenesisBlockHeader


----------------------------------------------------------------------------
-- IsGenesisHeader
----------------------------------------------------------------------------

-- | A class for genesis headers.
class IsHeader header => IsGenesisHeader header

instance HasDifficulty (Some IsGenesisHeader) where
    difficultyL = liftLensSome difficultyL

instance HasEpochIndex (Some IsGenesisHeader) where
    epochIndexL = liftLensSome epochIndexL

instance HasEpochOrSlot (Some IsGenesisHeader) where
    getEpochOrSlot = applySome getEpochOrSlot

instance HasPrevBlock (Some IsGenesisHeader) where
    prevBlockL = liftLensSome prevBlockL

instance HasHeaderHash (Some IsGenesisHeader) where
    headerHash = applySome headerHash

instance IsHeader        (Some IsGenesisHeader)
instance IsGenesisHeader (Some IsGenesisHeader)

instance IsGenesisHeader GenesisBlockHeader


----------------------------------------------------------------------------
-- IsMainHeader
----------------------------------------------------------------------------

{- | A class for main headers. In addition to 'IsHeader', provides:

  * 'headerSlotL'
  * 'headerLeaderKeyL'
  * 'blockVersionL'
  * 'softwareVersionL'
-}
class (IsHeader header
      ,HasBlockVersion header
      ,HasSoftwareVersion header) =>
      IsMainHeader header
  where
    -- | Id of the slot for which this block was generated.
    headerSlotL :: Lens' header SlotId
    -- | Public key of slot leader.
    headerLeaderKeyL :: Lens' header PublicKey

instance HasDifficulty (Some IsMainHeader) where
    difficultyL = liftLensSome difficultyL

instance HasEpochIndex (Some IsMainHeader) where
    epochIndexL = liftLensSome epochIndexL

instance HasEpochOrSlot (Some IsMainHeader) where
    getEpochOrSlot = applySome getEpochOrSlot

instance HasPrevBlock (Some IsMainHeader) where
    prevBlockL = liftLensSome prevBlockL

instance HasHeaderHash (Some IsMainHeader) where
    headerHash = applySome headerHash

instance HasBlockVersion (Some IsMainHeader) where
    blockVersionL = liftLensSome blockVersionL

instance HasSoftwareVersion (Some IsMainHeader) where
    softwareVersionL = liftLensSome softwareVersionL

instance IsHeader     (Some IsMainHeader)
instance IsMainHeader (Some IsMainHeader) where
    headerSlotL = liftLensSome headerSlotL
    headerLeaderKeyL = liftLensSome headerLeaderKeyL

instance IsMainHeader MainBlockHeader where
    headerSlotL = mainHeaderSlot
    headerLeaderKeyL = mainHeaderLeaderKey
