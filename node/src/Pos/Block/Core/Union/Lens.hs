-- | Lenses for general blockchain types.
--
-- Lenses whose name starts with `generalBlock' are from either 'GenesisBlock' and
-- 'MainBlock' to small parts of it. It makes it clear what exactly is stored in
-- 'GenesisBlock' and 'MainBlock'.

module Pos.Block.Core.Union.Lens
       (
         -- * Block EpochIndex
         generalBlockEpochIndex
       ) where

import           Universum

import           Control.Lens                (choosing)
import           Pos.Core                    (EpochIndex, siEpochL)

import           Pos.Block.Core.Genesis.Lens (genBlockEpoch)
import           Pos.Block.Core.Main.Lens    (mainBlockSlot)
import           Pos.Block.Core.Union.Types  (Block)

-- | General block @EpochIndex@. You give me a general @Block@, which is either
-- @GenesisBlock@ or @MainBlock@ and I'll return you the @EpochIndex@.
generalBlockEpochIndex :: forall ssc. Lens' (Block ssc) EpochIndex
generalBlockEpochIndex = choosing genBlockEpoch (mainBlockSlot . siEpochL)