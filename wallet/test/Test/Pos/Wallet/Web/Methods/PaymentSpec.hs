{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Wallet.Web.Methods.PaymentSpec
       ( spec
       ) where

import           Universum

import           Control.Exception.Safe (try)
import           Data.List ((!!), (\\))
import           Data.List.NonEmpty (fromList)
import           Formatting (build, sformat, (%))
import           Test.Hspec (Spec, beforeAll_, describe, runIO, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (arbitrary, choose, generate)
import           Test.QuickCheck.Monadic (pick)

import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Txp (Tx (..), TxAux (..), TxFee (..),
                     TxpConfiguration, _TxOut)
import           Pos.Chain.Update (bvdTxFeePolicy)
import           Pos.Client.Txp.Balances (getBalance)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), txToLinearFee)
import           Pos.Core (Address, Coin, TxFeePolicy (..), mkCoin, sumCoins,
                     unsafeGetCoin, unsafeSubCoin)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (PassPhrase, ProtocolMagic (..),
                     RequiresNetworkMagic (..))
import           Pos.DB.Class (MonadGState (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Wallet.Web.Account (myRootAddresses)
import           Pos.Wallet.Web.ClientTypes (Addr, CAccount (..), CId, CTx (..),
                     NewBatchPayment (..), Wal)
import           Servant.Server (ServantErr (..), err403)

import           Pos.Wallet.Web.Methods.Logic (getAccounts)
import           Pos.Wallet.Web.Methods.Payment (newPaymentBatch)
import qualified Pos.Wallet.Web.State.State as WS
import           Pos.Wallet.Web.State.Storage (AddressInfo (..), wamAddress)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getAccountAddrsOrThrow)

import           Pos.Util.Servant (encodeCType)
import           Pos.Util.Wlog (setupTestLogging)

import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Util.QuickCheck.Property (assertProperty, expectedOne,
                     maybeStopProperty, splitWord, stopProperty)
import           Test.Pos.Wallet.Web.Mode (WalletProperty, getSentTxs,
                     submitTxTestMode, walletPropertySpec)
import           Test.Pos.Wallet.Web.Util (deriveRandomAddress,
                     expectedAddrBalance, importSomeWallets,
                     mostlyEmptyPassphrases)


deriving instance Eq CTx

-- TODO remove HasCompileInfo when MonadWalletWebMode will be splitted.
spec :: Spec
spec = do
    runWithMagic RequiresNoMagic
    runWithMagic RequiresMagic

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = beforeAll_ setupTestLogging $
    withCompileInfo $ do
        pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
        describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
            withProvidedMagicConfig pm $ \genesisConfig txpConfig _ ->
                describe "Wallet.Web.Methods.Payment" $ modifyMaxSuccess (const 10) $ do
                describe "newPaymentBatch" $ do
                    describe "Submitting a payment when restoring" (rejectPaymentIfRestoringSpec genesisConfig txpConfig)
                    describe "One payment" (oneNewPaymentBatchSpec genesisConfig txpConfig)

data PaymentFixture = PaymentFixture {
      pswd        :: PassPhrase
    , dstWalIds   :: [CId Wal]
    , dstCAddrs   :: [CId Addr]
    , initBalance :: Coin
    , policy      :: InputSelectionPolicy
    , batch       :: NewBatchPayment
    , srcAddr     :: Address
    , walId       :: CId Wal
    , coins       :: [Coin]
}

-- | Generic block of code to be reused across all the different payment specs.
newPaymentFixture :: Genesis.Config -> WalletProperty PaymentFixture
newPaymentFixture genesisConfig = do
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    passphrases <- importSomeWallets genesisConfig mostlyEmptyPassphrases
    let l = length passphrases
    destLen <- pick $ choose (1, l)
    -- FIXME: we are sending to at most dstLen (which is small) because
    -- deriveRandomAddress is an expensive operation so it might
    -- take a longer time for test to complete for a longer lists
    (dstCAddrs, dstWalIds) <- fmap unzip $ replicateM destLen $ deriveRandomAddress nm passphrases
    rootsWIds <- lift (myRootAddresses nm)
    idx <- pick $ choose (0, l - 1)
    let walId = rootsWIds !! idx
    let pswd = passphrases !! idx
    let noOneAccount = sformat ("There is no one account for wallet: "%build) walId
    srcAccount <- maybeStopProperty noOneAccount =<< (lift $ (fmap fst . uncons) <$> getAccounts nm (Just walId))
    srcAccId <- lift $ decodeCTypeOrFail (caId srcAccount)

    ws <- WS.askWalletSnapshot
    srcAddr <- getAddress ws srcAccId
    -- Dunno how to get account's balances without CAccModifier
    initBalance <- getBalance (configGenesisData genesisConfig) srcAddr
    -- `div` 2 to leave money for tx fee
    let topBalance = unsafeGetCoin initBalance `div` 2
    coins <- pick $ map mkCoin <$> splitWord topBalance (fromIntegral destLen)
    policy <- pick arbitrary
    let batch = NewBatchPayment
                { npbFrom = encodeCType srcAccId
                , npbTo = fromList $ zip dstCAddrs coins
                , npbInputSelectionPolicy = policy
                }
    return PaymentFixture{..}
  where
    getAddress ws srcAccId =
        return . view wamAddress . adiWAddressMeta =<<
        expectedOne "address" =<<
        lift (getAccountAddrsOrThrow ws WS.Existing srcAccId)

-- | Assess that if we try to submit a payment when the wallet is restoring,
-- the backend prevents us from doing that.
rejectPaymentIfRestoringSpec :: HasConfigurations => Genesis.Config -> TxpConfiguration -> Spec
rejectPaymentIfRestoringSpec genesisConfig txpConfig =
    walletPropertySpec genesisConfig "should fail with 403" $ do
        PaymentFixture{..} <- newPaymentFixture genesisConfig
        res <- lift $ try (newPaymentBatch genesisConfig txpConfig submitTxTestMode pswd batch)
        liftIO $ shouldBe res (Left (err403 { errReasonPhrase = "Transaction creation is disabled when the wallet is restoring." }))

-- | Test one single, successful payment.
oneNewPaymentBatchSpec :: HasConfigurations => Genesis.Config -> TxpConfiguration -> Spec
oneNewPaymentBatchSpec genesisConfig txpConfig =
    walletPropertySpec genesisConfig oneNewPaymentBatchDesc $ do
        PaymentFixture{..} <- newPaymentFixture genesisConfig

        -- Force the wallet to be in a (fake) synced state
        db <- WS.askWalletDB
        randomSyncTip <- liftIO $ generate arbitrary
        WS.setWalletSyncTip db walId randomSyncTip

        void $ lift $ newPaymentBatch genesisConfig txpConfig submitTxTestMode pswd batch
        dstAddrs <- lift $ mapM decodeCTypeOrFail dstCAddrs
        txLinearPolicy <- lift $ (bvdTxFeePolicy <$> gsAdoptedBVData) <&> \case
            TxFeePolicyTxSizeLinear linear -> linear
            _                              -> error "unknown fee policy"
        txAux <- expectedOne "sent TxAux" =<< lift getSentTxs
        TxFee fee <- lift (runExceptT $ txToLinearFee txLinearPolicy txAux) >>= \case
            Left er -> stopProperty $ "Couldn't compute tx fee by tx, reason: " <> pretty er
            Right x -> pure x
        let outAddresses = map (fst . view _TxOut) $ toList $ _txOutputs $ taTx txAux
        let changeAddrs = outAddresses \\ dstAddrs
        assertProperty (length changeAddrs <= 1) $
            "Expected at most one change address"

        -- Validate balances
        let genesisData = configGenesisData genesisConfig
        mapM_ (uncurry (expectedAddrBalance genesisData)) $ zip dstAddrs coins
        when (policy == OptimizeForSecurity) $
            expectedAddrBalance genesisData srcAddr (mkCoin 0)
        changeBalance <- mkCoin . fromIntegral . sumCoins
            <$> mapM (getBalance genesisData) changeAddrs
        assertProperty (changeBalance <= initBalance `unsafeSubCoin` fee) $
            "Minimal tx fee isn't satisfied"

        ws' <- WS.askWalletSnapshot
        -- Validate that tx meta was added when transaction was processed
        forM_ (ordNub $ walId:dstWalIds) $ \wId -> do
            txMetas <- maybeStopProperty "Wallet doesn't exist" (WS.getWalletTxHistory ws' wId)
            void $ expectedOne "TxMeta for wallet" txMetas

        -- Validate change and used address
        -- TODO implement it when access
        -- to these addresses will be provided considering mempool
        -- expectedUserAddresses
        -- expectedChangeAddresses
  where
    oneNewPaymentBatchDesc =
        "Send money from one own address to multiple own addresses; " <>
        "check balances validity for destination addresses, source address and change address; " <>
        "validate history and used/change addresses"
