-- | Specification of 'Pos.Block.Logic.VAR'.

module Test.Pos.Block.Logic.VarSpec
       ( spec
       , maybeStopProperty
       ) where

import           Universum
import           Unsafe                    (unsafeHead)

import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)
import           Test.QuickCheck.Monadic   (PropertyM, stop)
import           Test.QuickCheck.Property  (Result (..), failed)

import           Pos.Block.Logic           (verifyBlocksPrefix)
import           Pos.Util.Chrono           (getOldestFirst)

import           Test.Pos.Block.Logic.Mode (BlockProperty)
import           Test.Pos.Block.Logic.Util (bpGenBlocks)

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

-- Note, 'fail' does the same thing, but:
-- • it's quite trivial, almost no copy-paste;
-- • it's 'fail' from 'Monad', not 'MonadFail';
-- • I am not a fan of 'fail'.
-- TODO: maybe these functions will become popular, then it makes
-- sense to move them somewhere.
stopProperty :: Monad m => Text -> PropertyM m a
stopProperty msg = stop failed {reason = toString msg}

maybeStopProperty :: Monad m => Text -> Maybe a -> PropertyM m a
maybeStopProperty msg =
    \case
        Nothing -> stopProperty msg
        Just x -> pure x

verifyEmptyMainBlock :: BlockProperty ()
verifyEmptyMainBlock = do
    emptyBlock <- fst . unsafeHead . getOldestFirst <$> bpGenBlocks (Just 1)
    whenLeftM (lift $ verifyBlocksPrefix (one emptyBlock)) stopProperty
