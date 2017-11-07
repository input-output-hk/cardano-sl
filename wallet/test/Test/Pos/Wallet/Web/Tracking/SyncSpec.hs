module Test.Pos.Wallet.Web.Tracking.SyncSpec
       ( spec
       ) where

import           Universum

import           Data.Default                 (def)
import           Test.Hspec                   (Spec, describe)
import           Test.Hspec.QuickCheck        (modifyMaxSuccess)
import           Test.QuickCheck              (choose)
import           Test.QuickCheck.Monadic      (pick)

import           Pos.Block.Logic              (rollbackBlocks)
import           Pos.Core                     (BlockCount (..), blkSecurityParam)
import           Pos.Crypto                   (emptyPassphrase)
import           Pos.Launcher                 (HasConfigurations)
import           Pos.Util.Chrono              (nonEmptyOldestFirst, toNewestFirst)
import           Pos.Util.CompileInfo         (HasCompileInfo, withCompileInfo)
import qualified Pos.Wallet.Web.State.State   as WS
import           Pos.Wallet.Web.State.Storage (WalletStorage (..))
import           Test.Pos.Block.Logic.Util    (EnableTxPayload (..), InplaceDB (..))
import           Test.Pos.Util                (assertProperty, withDefConfigurations)

import           Test.Pos.Wallet.Web.Mode     (walletPropertySpec)
import           Test.Pos.Wallet.Web.Util     (importSomeWallets, wpGenBlocks)

spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "Pos.Wallet.Web.Tracking.BListener" $ modifyMaxSuccess (const 10) $ do
    describe "Two applications and rollbacks" twoApplyTwoRollbacksSpec

twoApplyTwoRollbacksSpec :: (HasCompileInfo, HasConfigurations) => Spec
twoApplyTwoRollbacksSpec = walletPropertySpec twoApplyTwoRollbacksDesc $ do
    let k = fromIntegral blkSecurityParam :: Word64
    void $ importSomeWallets (pure emptyPassphrase)
    genesisWalletDB <- lift WS.getWalletStorage
    applyBlocksCnt1 <- pick $ choose (1, k `div` 2)
    applyBlocksCnt2 <- pick $ choose (1, k `div` 2)
    blunds1 <- wpGenBlocks (Just $ BlockCount applyBlocksCnt1) (EnableTxPayload True) (InplaceDB True)
    after1ApplyDB <- lift WS.getWalletStorage
    blunds2 <- wpGenBlocks (Just $ BlockCount applyBlocksCnt2) (EnableTxPayload True) (InplaceDB True)
    after2ApplyDB <- lift WS.getWalletStorage
    let toNE = fromMaybe (error "sequence of blocks are empty") . nonEmptyOldestFirst
    let to1Rollback = toNewestFirst $ toNE blunds2
    let to2Rollback = toNewestFirst $ toNE blunds1
    lift $ rollbackBlocks to1Rollback
    after1RollbackDB <- lift WS.getWalletStorage
    lift $ rollbackBlocks to2Rollback
    after2RollbackDB <- lift WS.getWalletStorage
    assertProperty (after1RollbackDB == after1ApplyDB)
        "wallet-db after first apply doesn't equal to wallet-db after first rollback"
    assertProperty (after2RollbackDB == genesisWalletDB)
        "genesis wallet-db doesn't equal to wallet-db after two rollbacks"

    -- Sanity checks
    assertProperty (_wsWalletInfos after1ApplyDB /= _wsWalletInfos genesisWalletDB)
        "something strange happened: genesis wallet infos equal to wallet infos after one applications"
    assertProperty (_wsWalletInfos after2ApplyDB /= _wsWalletInfos genesisWalletDB)
        "something strange happened: genesis wallet infos equal to wallet infos after two applications"
    assertProperty (_wsWalletInfos after1RollbackDB /= _wsWalletInfos genesisWalletDB)
        "something strange happened: genesis wallet infos equal to wallet info after first rollback"
  where
    twoApplyTwoRollbacksDesc =
        "Applications of two batches of blocks, " <>
        "then rollback second batch and compare wallet-db " <>
        "with wallet-db after first application.\n" <>
        "Rollback first batch of blocks and compare wallet-db with genesis one."
