-- | Specification of 'Pos.Block.Logic.VAR'.

module Test.Pos.Block.Logic.VarSpec
       ( spec
       ) where

import           Universum
import           Unsafe                    (unsafeHead)

import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)

import           Pos.Block.Logic           (verifyBlocksPrefix)
import           Pos.Util.Chrono           (getOldestFirst)

import           Test.Pos.Block.Logic.Mode (BlockProperty)
import           Test.Pos.Block.Logic.Util (bpGenBlocks)
import           Test.Pos.Util             (stopProperty)

spec :: Spec
spec = describe "Block.Logic.VAR" $ do
    describe "verifyBlocksPrefix" verifyBlocksPrefixSpec

----------------------------------------------------------------------------
-- verifyBlocksPrefix
----------------------------------------------------------------------------

verifyBlocksPrefixSpec :: Spec
verifyBlocksPrefixSpec = do
    -- Unfortunatelly, blocks generation is currently extremely slow.
    -- Maybe we will optimize it in future.
    modifyMaxSuccess (const 3) $
        prop verifyEmptyMainBlockDesc verifyEmptyMainBlock
  where
    verifyEmptyMainBlockDesc =
        "verification of consistent empty main block " <>
        "created by the leader of the 0-th slot " <>
        "always succeeds for initial GState"

verifyEmptyMainBlock :: BlockProperty ()
verifyEmptyMainBlock = do
    emptyBlock <- fst . unsafeHead . getOldestFirst <$> bpGenBlocks (Just 1)
    whenLeftM (lift $ verifyBlocksPrefix (one emptyBlock)) stopProperty
