-- | Specification of 'Pos.Block.Logic.VAR'.

module Test.Pos.Block.Logic.VarSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck.Monadic   (assert)

import           Test.Pos.Block.Logic.Mode (BlockProperty)

spec :: Spec
spec = describe "Block.Logic.VAR" $ do
    describe "verifyBlocksPrefix" verifyBlocksPrefixSpec

----------------------------------------------------------------------------
-- verifyBlocksPrefix
----------------------------------------------------------------------------

verifyBlocksPrefixSpec :: Spec
verifyBlocksPrefixSpec = do
    prop verifyEmptyMainBlockDesc verifyEmptyMainBlock
  where
    verifyEmptyMainBlockDesc =
        "verification of consistent empty main block " <>
        "always succeeds for initial GState"

-- TODO
verifyEmptyMainBlock :: BlockProperty ()
verifyEmptyMainBlock = do
    assert True
