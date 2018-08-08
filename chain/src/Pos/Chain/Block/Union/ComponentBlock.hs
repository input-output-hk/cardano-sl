module Pos.Chain.Block.Union.ComponentBlock
       ( ComponentBlock (..)
       ) where

import           Universum

import           Control.Lens (lens)

import           Pos.Chain.Block.Genesis ()
import           Pos.Chain.Block.Main ()
import           Pos.Chain.Block.Union.Types (HasHeaderHash (..),
                     HasPrevBlock (..), IsGenesisHeader, IsMainHeader (..))
import           Pos.Core.Slotting (EpochOrSlot (..), HasEpochIndex (..),
                     HasEpochOrSlot (..))
import           Pos.Util.Some (Some)

-- | Representation of 'Block' passed to a component.
data ComponentBlock payload =
    ComponentBlockGenesis (Some IsGenesisHeader)
    | ComponentBlockMain !(Some IsMainHeader) !payload

instance HasHeaderHash (ComponentBlock a) where
    headerHash (ComponentBlockGenesis genesisHeader) = headerHash genesisHeader
    headerHash (ComponentBlockMain mainHeader _)     = headerHash mainHeader

instance HasPrevBlock (ComponentBlock a) where
    prevBlockL = lens getter setter
      where
        getter (ComponentBlockGenesis genesisHeader) = genesisHeader ^. prevBlockL
        getter (ComponentBlockMain mainHeader _)     = mainHeader ^. prevBlockL
        setter (ComponentBlockGenesis genesisHeader) e =
            ComponentBlockGenesis (genesisHeader & prevBlockL .~ e)
        setter (ComponentBlockMain mainHeader payload) e =
            ComponentBlockMain (mainHeader & prevBlockL .~ e) payload

instance HasEpochIndex (ComponentBlock a) where
    epochIndexL = lens getter setter
        where
            getter (ComponentBlockGenesis genesisHeader) = genesisHeader ^. epochIndexL
            getter (ComponentBlockMain mainHeader _)     = mainHeader ^. epochIndexL
            setter (ComponentBlockGenesis genesisHeader) e =
                ComponentBlockGenesis (genesisHeader & epochIndexL .~ e)
            setter (ComponentBlockMain mainHeader payload) e =
                ComponentBlockMain (mainHeader & epochIndexL .~ e) payload

instance HasEpochOrSlot (ComponentBlock a) where
    getEpochOrSlot (ComponentBlockMain a _)  = EpochOrSlot $ Right $ a ^. headerSlotL
    getEpochOrSlot (ComponentBlockGenesis a) = EpochOrSlot $ Left $ a ^. epochIndexL
