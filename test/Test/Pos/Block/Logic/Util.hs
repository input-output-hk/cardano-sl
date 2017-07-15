-- | Utilities for block logic testing.

module Test.Pos.Block.Logic.Util
       ( bpGenBlocks
       ) where

import           Universum

import           Test.QuickCheck.Gen       (sized)
import           Test.QuickCheck.Monadic   (pick)

import           Pos.Block.Types           (Blund)
import           Pos.Generator.Block       (BlockGenParams (..), genBlocks)
import           Pos.Ssc.GodTossing        (SscGodTossing)
import           Pos.Util.Chrono           (OldestFirst)

import           Pos.Util.Util             (HasLens (..))
import           Test.Pos.Block.Logic.Mode (BlockProperty, BlockTestContextTag,
                                            tpAllSecrets)

-- | Generate arbitrary valid blocks inside 'BlockProperty'.
bpGenBlocks :: BlockProperty (OldestFirst [] (Blund SscGodTossing))
bpGenBlocks = do
    allSecrets <- lift $ view (lensOf @BlockTestContextTag . tpAllSecrets)
    let genBlockGenParams s =
            pure
                BlockGenParams
                {_bgpSecrets = allSecrets, _bgpBlockCount = fromIntegral s}
    params <- pick $ sized genBlockGenParams
    lift (genBlocks params)
