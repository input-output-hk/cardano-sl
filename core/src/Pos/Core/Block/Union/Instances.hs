{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | United miscellaneous functionality.

module Pos.Core.Block.Union.Instances
       ( getBlockHeader
       , blockHeader
       ) where

import           Universum

import           Control.Lens (Getter, choosing, lens, to)
import qualified Data.Text.Buildable as Buildable

import           Pos.Binary.Class (Bi)
import           Pos.Core.Block.Blockchain (GenericBlock (..))
import           Pos.Core.Block.Genesis ()
import           Pos.Core.Block.Main ()
import           Pos.Core.Block.Union.Types (Block, BlockHeader (..), ComponentBlock (..),
                                             blockHeaderHash, choosingBlockHeader)
import           Pos.Core.Class (HasDifficulty (..), HasEpochIndex (..), HasEpochOrSlot (..),
                                 HasHeaderHash (..), HasPrevBlock (..), IsHeader, IsMainHeader (..))
import           Pos.Core.Slotting.Types (EpochOrSlot (..))

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance Bi BlockHeader =>
         Buildable BlockHeader where
    build = \case
        BlockHeaderGenesis bhg -> Buildable.build bhg
        BlockHeaderMain    bhm -> Buildable.build bhm

----------------------------------------------------------------------------
-- HasHeaderHash
----------------------------------------------------------------------------

instance Bi BlockHeader =>
         HasHeaderHash BlockHeader where
    headerHash = blockHeaderHash

instance Bi BlockHeader =>
         HasHeaderHash Block where
    headerHash = blockHeaderHash . getBlockHeader

-- | Take 'BlockHeader' from either 'GenesisBlock' or 'MainBlock'.
getBlockHeader :: Block -> BlockHeader
getBlockHeader = \case
    Left  gb -> BlockHeaderGenesis (_gbHeader gb)
    Right mb -> BlockHeaderMain    (_gbHeader mb)

blockHeader :: Getter Block BlockHeader
blockHeader = to getBlockHeader

instance HasHeaderHash (ComponentBlock a) where
    headerHash (ComponentBlockGenesis genesisHeader) = headerHash genesisHeader
    headerHash (ComponentBlockMain mainHeader _)     = headerHash mainHeader

----------------------------------------------------------------------------
-- HasDifficulty
----------------------------------------------------------------------------

instance HasDifficulty BlockHeader where
    difficultyL = choosingBlockHeader difficultyL difficultyL

instance HasDifficulty Block where
    difficultyL = choosing difficultyL difficultyL

-----------------------------------------------------------------------------
--- HasEpochIndex
-----------------------------------------------------------------------------

instance HasEpochIndex BlockHeader where
    epochIndexL = choosingBlockHeader epochIndexL epochIndexL

----------------------------------------------------------------------------
-- IsHeader
----------------------------------------------------------------------------

instance Bi BlockHeader => IsHeader BlockHeader

----------------------------------------------------------------------------
-- HasPrevBlock
----------------------------------------------------------------------------

instance HasPrevBlock BlockHeader where
    prevBlockL = choosingBlockHeader prevBlockL prevBlockL

instance HasPrevBlock (ComponentBlock a) where
    prevBlockL = lens getter setter
      where
        getter (ComponentBlockGenesis genesisHeader) = genesisHeader ^. prevBlockL
        getter (ComponentBlockMain mainHeader _)     = mainHeader ^. prevBlockL
        setter (ComponentBlockGenesis genesisHeader) e =
            ComponentBlockGenesis (genesisHeader & prevBlockL .~ e)
        setter (ComponentBlockMain mainHeader payload) e =
            ComponentBlockMain (mainHeader & prevBlockL .~ e) payload

----------------------------------------------------------------------------
-- HasEpochIndex
----------------------------------------------------------------------------

instance HasEpochIndex (ComponentBlock a) where
    epochIndexL = lens getter setter
        where
            getter (ComponentBlockGenesis genesisHeader) = genesisHeader ^. epochIndexL
            getter (ComponentBlockMain mainHeader _)     = mainHeader ^. epochIndexL
            setter (ComponentBlockGenesis genesisHeader) e =
                ComponentBlockGenesis (genesisHeader & epochIndexL .~ e)
            setter (ComponentBlockMain mainHeader payload) e =
                ComponentBlockMain (mainHeader & epochIndexL .~ e) payload

----------------------------------------------------------------------------
-- HasEpochOrSlot
----------------------------------------------------------------------------

instance HasEpochOrSlot BlockHeader where
    getEpochOrSlot = view (choosingBlockHeader (to getEpochOrSlot) (to getEpochOrSlot))

instance HasEpochOrSlot (ComponentBlock a) where
    getEpochOrSlot (ComponentBlockMain a _)  = EpochOrSlot $ Right $ a ^. headerSlotL
    getEpochOrSlot (ComponentBlockGenesis a) = EpochOrSlot $ Left $ a ^. epochIndexL
