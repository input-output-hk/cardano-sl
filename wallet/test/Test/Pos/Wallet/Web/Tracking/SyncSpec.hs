{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Pos.Wallet.Web.Tracking.SyncSpec
       ( spec
       ) where

import           Universum

import qualified Data.HashSet as HS
import           Data.List (intersect, (\\))
import           Pos.Client.KeyStorage (getSecretKeysPlain)
import           Test.Hspec (Spec, describe, runIO, xdescribe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Property, arbitrary, choose, generate, oneof,
                                  sublistOf, suchThat, vectorOf, (===))
import           Test.QuickCheck.Monadic (pick)

import           Pos.Arbitrary.Wallet.Web.ClientTypes ()
import           Pos.Block.Logic (rollbackBlocks)
import           Pos.Core (Address, BlockCount (..), blkSecurityParam)
import           Pos.Core.Chrono (nonEmptyOldestFirst, toNewestFirst)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..), emptyPassphrase)
import           Pos.Launcher (HasConfigurations)

import qualified Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.State.Storage (WalletStorage (..))
import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)
import           Pos.Wallet.Web.Tracking.Sync (evalChange, syncWalletWithBlockchain)
import           Pos.Wallet.Web.Tracking.Types (newSyncRequest)

-- import           Pos.Wallet.Web.ClientTypes ()
-- import qualified Pos.Wallet.Web.State.State as WS
-- import           Pos.Wallet.Web.State.Storage (WalletStorage (..))
-- import           Pos.Wallet.Web.Tracking.Sync (evalChange)


import           Test.Pos.Block.Logic.Util (EnableTxPayload (..), InplaceDB (..))
import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Util.QuickCheck.Property (assertProperty)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)
import           Test.Pos.Wallet.Web.Util (importSomeWallets, wpGenBlocks)

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ do
    describe "Pos.Wallet.Web.Tracking.BListener" $ modifyMaxSuccess (const 10) $ do
        describe "Two applications and rollbacks" (twoApplyTwoRollbacksSpec pm)
    xdescribe "Pos.Wallet.Web.Tracking.evalChange (pending, CSL-2473)" $ do
        prop evalChangeDiffAccountsDesc evalChangeDiffAccounts
        prop evalChangeSameAccountsDesc evalChangeSameAccounts
  where
    evalChangeDiffAccountsDesc =
      "An outgoing transaction to another account."
    evalChangeSameAccountsDesc =
      "Outgoing transaction from account to the same account."

twoApplyTwoRollbacksSpec :: HasConfigurations => ProtocolMagic -> Spec
twoApplyTwoRollbacksSpec pm = walletPropertySpec twoApplyTwoRollbacksDesc $ do
    let k = fromIntegral blkSecurityParam :: Word64
    -- During these tests we need to manually switch back to the old synchronous
    -- way of restoring.
    void $ importSomeWallets (pure emptyPassphrase)
    sks <- lift getSecretKeysPlain
    lift $ forM_ sks $ \s -> syncWalletWithBlockchain (newSyncRequest (eskToWalletDecrCredentials s))

    -- Testing starts here
    genesisWalletDB <- lift WS.askWalletSnapshot
    applyBlocksCnt1 <- pick $ choose (1, k `div` 2)
    applyBlocksCnt2 <- pick $ choose (1, k `div` 2)
    blunds1 <- wpGenBlocks pm
                           (Just $ BlockCount applyBlocksCnt1)
                           (EnableTxPayload True)
                           (InplaceDB True)
    after1ApplyDB <- lift WS.askWalletSnapshot
    blunds2 <- wpGenBlocks pm
                           (Just $ BlockCount applyBlocksCnt2)
                           (EnableTxPayload True)
                           (InplaceDB True)
    after2ApplyDB <- lift WS.askWalletSnapshot
    let toNE = fromMaybe (error "sequence of blocks are empty") . nonEmptyOldestFirst
    let to1Rollback = toNewestFirst $ toNE blunds2
    let to2Rollback = toNewestFirst $ toNE blunds1
    lift $ rollbackBlocks pm to1Rollback
    after1RollbackDB <- lift WS.askWalletSnapshot
    lift $ rollbackBlocks pm to2Rollback
    after2RollbackDB <- lift WS.askWalletSnapshot
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
    { inpAddrs    :: [WS.WAddressMeta]
    , outAddrs    :: [WS.WAddressMeta]
    , changeAddrs :: HashSet Address
    , usedAddrs   :: HashSet Address
    } deriving Show

newtype AddressesFromDiffAccounts = AddressesFromDiffAccounts InpOutChangeUsedAddresses
    deriving Show

instance Arbitrary AddressesFromDiffAccounts where
    arbitrary = do
        (wId1, accIdx1) <- arbitrary
        let genAddrs1 n = map (uncurry $ WS.WAddressMeta wId1 accIdx1) <$> vectorOf n arbitrary
        inpAddrs <- choose (1, 5) >>= genAddrs1
        changeAddrsL <- (choose (1, 3) >>= genAddrs1)
          `suchThat` (\x -> null $ x `intersect` inpAddrs)
        let outAddrs = changeAddrsL
        let changeAddrs = HS.fromList $ map (view WS.wamAddress) changeAddrsL
        let usedAddrs = HS.fromList $ map (view WS.wamAddress) inpAddrs
        pure $ AddressesFromDiffAccounts $ InpOutUsedAddresses {..}

evalChangeDiffAccounts :: AddressesFromDiffAccounts -> Property
evalChangeDiffAccounts (AddressesFromDiffAccounts InpOutUsedAddresses {..}) =
   changeAddrs === HS.fromList (evalChange usedAddrs inpAddrs outAddrs False)

-- | newtype defined so that its Arbitrary instance can set the stage for
-- 'evalChangeSameAccounts'. The 'changeAddrs' field will always be set so
-- that it should equal
-- 'HS.fromList (evalChange usedAddrs inpAddrs outAddrs True)'.
newtype AddressesFromSameAccounts = AddressesFromSameAccounts InpOutChangeUsedAddresses
    deriving Show

instance Arbitrary AddressesFromSameAccounts where
    arbitrary = do
        wId <- arbitrary
        accIdx <- arbitrary
        -- generate n WAddressMeta terms each with the same wallet and account
        -- identifier ('wId' and 'accIdx' above) subject to a predicate.
        -- That predicate allows us to ensure that the output address
        -- ('outAddrs') are disjiont under 'WAddressMeta' equality from the
        -- input addresses, which is essential for the test.
        let genAddrs p n = vectorOf n $
                (uncurry (WS.WAddressMeta wId accIdx) <$> arbitrary)
                `suchThat` p
        inpAddrs <- choose (1, 5) >>= genAddrs (const True)
        outAddrs <- choose (1, 5) >>= genAddrs (not . flip elem inpAddrs)
        -- Throw on a bunch of arbitrary extra used addresses, but make sure
        -- they are not 'WAddressMeta'-equal to any existing ones!
        usedBase <- (inpAddrs ++) <$> (do
            n <- choose (1, 10)
            let allAddrs = inpAddrs ++ outAddrs
                condition = not . flip elem allAddrs
            vectorOf n $ arbitrary `suchThat` condition)
        (changeAddrs, extraUsed) <- oneof [
            -- Case when all outputs addresses are fresh and
            -- weren't mentioned in the blockchain.
            -- Change addresses should be empty.
            pure (mempty, [])
            -- Otherwise, there's at least one non-change address in the
            -- outputs. Every address that we don't put into the second
            -- component (which goes into the set of all used) should appear as
            -- a change address.
            , do
                if length outAddrs == 1
                then pure (mempty, [])
                else do
                    -- Problem case is when ext is all of 'outAddrs'.
                    ext <- sublistOf outAddrs `suchThat` (not . null)
                    pure (HS.fromList $ map (view WS.wamAddress) (outAddrs \\ ext), ext)
            ]
        let usedAddrs = HS.fromList $ map (view WS.wamAddress) $ usedBase ++ extraUsed
        pure $ AddressesFromSameAccounts $ InpOutUsedAddresses {..}

evalChangeSameAccounts :: AddressesFromSameAccounts -> Property
evalChangeSameAccounts (AddressesFromSameAccounts InpOutUsedAddresses {..}) =
   changeAddrs === HS.fromList (evalChange usedAddrs inpAddrs outAddrs True)
