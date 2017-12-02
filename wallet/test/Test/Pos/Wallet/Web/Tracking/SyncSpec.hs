module Test.Pos.Wallet.Web.Tracking.SyncSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import qualified Data.HashSet as HS
import           Data.List ((\\))
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Property, choose, oneof, sublistOf, suchThat,
                                  vectorOf, (===))
import           Test.QuickCheck.Monadic (pick)

import           Pos.Arbitrary.Wallet.Web.ClientTypes ()
import           Pos.Block.Logic (rollbackBlocks)
import           Pos.Core (BlockCount (..), blkSecurityParam)
import           Pos.Crypto (emptyPassphrase)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.Chrono (nonEmptyOldestFirst, toNewestFirst)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.ClientTypes (Addr, CId, CWAddressMeta (..))
import qualified Pos.Wallet.Web.State.State as WS
import           Pos.Wallet.Web.State.Storage (WalletStorage (..))
import           Pos.Wallet.Web.Tracking.Sync (evalChange)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..), InplaceDB (..))
import           Test.Pos.Util (assertProperty, withDefConfigurations)

import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)
import           Test.Pos.Wallet.Web.Util (importSomeWallets, wpGenBlocks)

spec :: Spec
spec = withCompileInfo def $ withDefConfigurations $ do
    describe "Pos.Wallet.Web.Tracking.BListener" $ modifyMaxSuccess (const 10) $ do
        describe "Two applications and rollbacks" twoApplyTwoRollbacksSpec
    describe "Pos.Wallet.Web.Tracking.evalChange" $ do
        prop evalChangeDiffAccountsDesc evalChangeDiffAccounts
        prop evalChangeSameAccountsDesc evalChangeSameAccounts
  where
    evalChangeDiffAccountsDesc =
      "An outgoing transaction to another account."
    evalChangeSameAccountsDesc =
      "Outgoing transcation from account to the same account."

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

----------------------------------------------------------------------------
-- evalChange properties
----------------------------------------------------------------------------

data InpOutChangeUsedAddresses = InpOutUsedAddresses
    { inpAddrs    :: [CWAddressMeta]
    , outAddrs    :: [CWAddressMeta]
    , changeAddrs :: HashSet (CId Addr)
    , usedAddrs   :: HashSet (CId Addr)
    } deriving Show

newtype AddressesFromDiffAccounts = AddressesFromDiffAccounts InpOutChangeUsedAddresses
    deriving Show

instance Arbitrary AddressesFromDiffAccounts where
    arbitrary = do
        (wId1, accIdx1) <- arbitrary
        let genAddrs1 n = map (uncurry $ CWAddressMeta wId1 accIdx1) <$> vectorOf n arbitrary
        inpAddrs <- choose (1, 5) >>= genAddrs1
        changeAddrsL <- choose (1, 3) >>= genAddrs1
        let outAddrs = changeAddrsL
        let changeAddrs = HS.fromList $ map cwamId changeAddrsL
        let usedAddrs = HS.fromList $ map cwamId inpAddrs
        pure $ AddressesFromDiffAccounts $ InpOutUsedAddresses {..}

evalChangeDiffAccounts :: AddressesFromDiffAccounts -> Property
evalChangeDiffAccounts (AddressesFromDiffAccounts InpOutUsedAddresses {..}) =
   changeAddrs === HS.fromList (evalChange usedAddrs inpAddrs outAddrs False)

newtype AddressesFromSameAccounts = AddressesFromSameAccounts InpOutChangeUsedAddresses
    deriving Show

instance Arbitrary AddressesFromSameAccounts where
    arbitrary = do
        wId <- arbitrary
        accIdx <- arbitrary
        let genAddrs n = map (uncurry $ CWAddressMeta wId accIdx) <$> vectorOf n arbitrary
        inpAddrs <- choose (1, 5) >>= genAddrs
        outAddrs <- choose (1, 5) >>= genAddrs
        usedBase <- (inpAddrs ++) <$> (choose (1, 10) >>= flip vectorOf arbitrary)
        (changeAddrs, extraUsed) <- oneof [
            -- Case when all outputs addresses are fresh and
            -- weren't mentioned in the blockchain
            pure (mempty, [])
            , do
                if length outAddrs == 1 then pure (mempty, [])
                else do
                    ext <- sublistOf outAddrs `suchThat` (not . null)
                    pure (HS.fromList $ map cwamId (outAddrs \\ ext), ext)
            ]
        let usedAddrs = HS.fromList $ map cwamId $ usedBase ++ extraUsed
        pure $ AddressesFromSameAccounts $ InpOutUsedAddresses {..}

evalChangeSameAccounts :: AddressesFromSameAccounts -> Property
evalChangeSameAccounts (AddressesFromSameAccounts InpOutUsedAddresses {..}) =
   changeAddrs === HS.fromList (evalChange usedAddrs inpAddrs outAddrs True)
