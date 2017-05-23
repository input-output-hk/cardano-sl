{-# LANGUAGE CPP             #-}
-- needed for stylish-haskell :(
{-# LANGUAGE TemplateHaskell #-}

module Pos.Core.Class
       (
       -- * Classes for overloaded accessors
         HasPrevBlock (..)
       , HasDifficulty (..)
       , HasBlockVersion (..)
       , HasSoftwareVersion (..)
       , HasHeaderHash (..)
       , headerHashG
       , HasEpochIndex (..)
       , HasEpochOrSlot (..)
       , epochOrSlotG

       -- * Classes for headers
       , IsHeader
       , IsGenesisHeader
       , IsMainHeader (..)
       ) where

import           Universum

import           Control.Lens   (Getter, choosing, lens, to)

import           Pos.Core.Types (BlockVersion, ChainDifficulty, EpochIndex,
                                 EpochOrSlot (..), HeaderHash, SlotId, SoftwareVersion,
                                 siEpoch)
import           Pos.Crypto     (PublicKey)
import           Pos.Util.Util  (Some, applySome, liftLensSome)

#define SOME_LENS_CLASS(HAS, LENS, CL)                       \
    instance HAS (Some CL) where LENS = liftLensSome LENS
#define SOME_FUNC_CLASS(HAS, FUNC, CL)                       \
    instance HAS (Some CL) where FUNC = applySome FUNC

----------------------------------------------------------------------------
-- Classes for overloaded accessors
----------------------------------------------------------------------------

-- HasPrevBlock
-- | Class for something that has previous block (lens to 'Hash' for this block).
class HasPrevBlock s where
    prevBlockL :: Lens' s HeaderHash

SOME_LENS_CLASS(HasPrevBlock, prevBlockL, HasPrevBlock)

instance (HasPrevBlock s, HasPrevBlock s') =>
         HasPrevBlock (Either s s') where
    prevBlockL = choosing prevBlockL prevBlockL


-- Perhaps it is not the best instance.
instance {-# OVERLAPPABLE #-} HasPrevBlock s => HasPrevBlock (s, z) where
    prevBlockL = _1 . prevBlockL

-- HasDifficulty
class HasDifficulty a where
    difficultyL :: Lens' a ChainDifficulty

SOME_LENS_CLASS(HasDifficulty, difficultyL, HasDifficulty)

-- HasBlockVersion
class HasBlockVersion a where
    blockVersionL :: Lens' a BlockVersion

SOME_LENS_CLASS(HasBlockVersion, blockVersionL, HasBlockVersion)

-- HasSoftwareVersion
class HasSoftwareVersion a where
    softwareVersionL :: Lens' a SoftwareVersion

SOME_LENS_CLASS(HasSoftwareVersion, softwareVersionL, HasSoftwareVersion)

-- HasHeaderHash
class HasHeaderHash a where
    headerHash :: a -> HeaderHash

SOME_FUNC_CLASS(HasHeaderHash, headerHash, HasHeaderHash)

instance HasHeaderHash HeaderHash where
    headerHash = identity

headerHashG :: HasHeaderHash a => Getter a HeaderHash
headerHashG = to headerHash

-- HasEpochIndex
class HasEpochIndex a where
    epochIndexL :: Lens' a EpochIndex

SOME_LENS_CLASS(HasEpochIndex, epochIndexL, HasEpochIndex)

instance HasEpochIndex SlotId where
    epochIndexL = lens siEpoch (\s a -> s {siEpoch = a})

instance (HasEpochIndex a, HasEpochIndex b) =>
         HasEpochIndex (Either a b) where
    epochIndexL = choosing epochIndexL epochIndexL

-- HasEpochOrSlot
class HasEpochOrSlot a where
    getEpochOrSlot :: a -> EpochOrSlot

SOME_FUNC_CLASS(HasEpochOrSlot, getEpochOrSlot, HasEpochOrSlot)

epochOrSlotG :: HasEpochOrSlot a => Getter a EpochOrSlot
epochOrSlotG = to getEpochOrSlot

instance HasEpochOrSlot EpochIndex where
    getEpochOrSlot = EpochOrSlot . Left
instance HasEpochOrSlot SlotId where
    getEpochOrSlot = EpochOrSlot . Right
instance HasEpochOrSlot EpochOrSlot where
    getEpochOrSlot = identity
instance (HasEpochOrSlot a, HasEpochOrSlot b) =>
         HasEpochOrSlot (Either a b) where
    getEpochOrSlot = either getEpochOrSlot getEpochOrSlot

----------------------------------------------------------------------------
-- Classes for headers
----------------------------------------------------------------------------

-- Add (..) to export list when IsHeader or IsGenesisHeader get any methods

{- | A class that lets subpackages use some fields from headers without
depending on cardano-sl:

  * 'difficultyL'
  * 'epochIndexL'
  * 'epochOrSlotG'
  * 'prevBlockL'
  * 'headerHashG'
-}
class (HasDifficulty header
      ,HasEpochIndex header
      ,HasEpochOrSlot header
      ,HasPrevBlock header
      ,HasHeaderHash header) =>
      IsHeader header

SOME_LENS_CLASS(HasDifficulty,  difficultyL,    IsHeader)
SOME_LENS_CLASS(HasEpochIndex,  epochIndexL,    IsHeader)
SOME_FUNC_CLASS(HasEpochOrSlot, getEpochOrSlot, IsHeader)
SOME_LENS_CLASS(HasPrevBlock,   prevBlockL,     IsHeader)
SOME_FUNC_CLASS(HasHeaderHash,  headerHash,     IsHeader)

instance IsHeader (Some IsHeader)

-- | A class for genesis headers.
class IsHeader header => IsGenesisHeader header

SOME_LENS_CLASS(HasDifficulty,  difficultyL,    IsGenesisHeader)
SOME_LENS_CLASS(HasEpochIndex,  epochIndexL,    IsGenesisHeader)
SOME_FUNC_CLASS(HasEpochOrSlot, getEpochOrSlot, IsGenesisHeader)
SOME_LENS_CLASS(HasPrevBlock,   prevBlockL,     IsGenesisHeader)
SOME_FUNC_CLASS(HasHeaderHash,  headerHash,     IsGenesisHeader)

instance IsHeader        (Some IsGenesisHeader)
instance IsGenesisHeader (Some IsGenesisHeader)

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

SOME_LENS_CLASS(HasDifficulty,      difficultyL,      IsMainHeader)
SOME_LENS_CLASS(HasEpochIndex,      epochIndexL,      IsMainHeader)
SOME_FUNC_CLASS(HasEpochOrSlot,     getEpochOrSlot,   IsMainHeader)
SOME_LENS_CLASS(HasPrevBlock,       prevBlockL,       IsMainHeader)
SOME_FUNC_CLASS(HasHeaderHash,      headerHash,       IsMainHeader)
SOME_LENS_CLASS(HasBlockVersion,    blockVersionL,    IsMainHeader)
SOME_LENS_CLASS(HasSoftwareVersion, softwareVersionL, IsMainHeader)

instance IsHeader     (Some IsMainHeader)
instance IsMainHeader (Some IsMainHeader) where
    headerSlotL = liftLensSome headerSlotL
    headerLeaderKeyL = liftLensSome headerLeaderKeyL
