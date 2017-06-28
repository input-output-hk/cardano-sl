-- | Specification of 'Pos.Block.Logic.VAR'.

module Test.Pos.Block.Logic.VarSpec
       ( spec
       ) where

import           Universum

import           Control.Lens              (at)
import qualified Data.List.NonEmpty        as NE
import qualified Ether
import           Formatting                (sformat, (%))
import           Serokell.Util             (listJson)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck.Monadic   (PropertyM, stop)
import           Test.QuickCheck.Property  (Result (..), failed)

import           Pos.Block.Core            (MainBlock, emptyMainBody, mkMainBlock)
import           Pos.Block.Logic           (verifyBlocksPrefix)
import           Pos.Core                  (SlotId (..))
import           Pos.DB.DB                 (getTipHeader)
import           Pos.Lrc                   (getLeaders)
import           Pos.Ssc.GodTossing        (SscGodTossing)

import           Test.Pos.Block.Logic.Mode (BlockProperty, TestParams (..))

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
    genesisLeaders <-
        maybeStopProperty "no genesis leaders" =<< lift (getLeaders 0)
    let theLeader = NE.head genesisLeaders
    idToSecret <- lift (Ether.asks' tpSecretKeys)
    let unknownLeaderMsg =
            sformat
                ("the secret key of the leader is unknown, leaders are: "
                 %listJson)
                genesisLeaders
    theSecretKey <-
        maybeStopProperty unknownLeaderMsg (idToSecret ^. at theLeader)
    tipHeader <- lift (getTipHeader @SscGodTossing)
    let slot0 = SlotId 0 minBound
    let mainBlock :: Either Text (MainBlock SscGodTossing)
        mainBlock =
            mkMainBlock
                (Just tipHeader)
                slot0
                theSecretKey
                Nothing
                (emptyMainBody minBound)
    block <- either stopProperty (pure . Right) mainBlock
    whenLeftM (lift $ verifyBlocksPrefix (one block)) stopProperty
