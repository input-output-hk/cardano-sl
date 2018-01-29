module Test.Pos.Wallet.Web.Methods.PaymentSpec
       ( spec
       ) where

import           Nub (ordNub)
import           Universum

import           Data.Default (def)
import           Data.List ((!!), (\\))
import           Data.List.NonEmpty (fromList)
import           Formatting (build, sformat, (%))
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (arbitrary, choose)
import           Test.QuickCheck.Monadic (pick)

import           Pos.Client.Txp.Balances (getBalance)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), txToLinearFee)
import           Pos.Core (TxFeePolicy (..), bvdTxFeePolicy, mkCoin, sumCoins, unsafeGetCoin,
                           unsafeSubCoin)
import           Pos.Core.Txp (Tx (..), TxAux (..), _TxOut)
import           Pos.DB.Class (MonadGState (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Txp (TxFee (..))
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Account (myRootAddresses)
import           Pos.Wallet.Web.ClientTypes (CAccount (..), CWAddressMeta (..),
                                             NewBatchPayment (..))

import           Pos.Wallet.Web.Methods.Logic (getAccounts)
import           Pos.Wallet.Web.Methods.Payment (newPaymentBatch)
import qualified Pos.Wallet.Web.State.State as WS
import           Pos.Wallet.Web.State.Storage (AddressInfo (..))
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getAccountAddrsOrThrow)
import           Test.Pos.Util (assertProperty, expectedOne, maybeStopProperty, splitWord,
                                stopProperty, withDefConfigurations)

import           Pos.Util.Servant (encodeCType)
import           Test.Pos.Wallet.Web.Mode (getSentTxs, walletPropertySpec)
import           Test.Pos.Wallet.Web.Util (deriveRandomAddress, expectedAddrBalance,
                                           importSomeWallets, mostlyEmptyPassphrases)


-- TODO remove HasCompileInfo when MonadWalletWebMode will be splitted.
spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "Wallet.Web.Methods.Payment" $ modifyMaxSuccess (const 10) $ do
    describe "newPaymentBatch" $ do
        describe "One payment" oneNewPaymentBatchSpec

oneNewPaymentBatchSpec :: (HasCompileInfo, HasConfigurations) => Spec
oneNewPaymentBatchSpec = walletPropertySpec oneNewPaymentBatchDesc $ do
    passphrases <- importSomeWallets mostlyEmptyPassphrases
    let l = length passphrases
    destLen <- pick $ choose (1, l)
    -- FIXME: we are sending to at most dstLen (which is small) because
    -- deriveRandomAddress is an expensive operation so it might
    -- take a longer time for test to complete for a longer lists
    (dstCAddrs, dstWalIds) <- fmap unzip $ replicateM destLen $ deriveRandomAddress passphrases
    rootsWIds <- lift myRootAddresses
    idx <- pick $ choose (0, l - 1)
    let walId = rootsWIds !! idx
    let pswd = passphrases !! idx
    let noOneAccount = sformat ("There is no one account for wallet: "%build) walId
    srcAccount <- maybeStopProperty noOneAccount =<< (lift $ head <$> getAccounts (Just walId))
    srcAccId <- lift $ decodeCTypeOrFail (caId srcAccount)

    srcAddr <- getAddress srcAccId
    -- Dunno how to get account's balances without CAccModifier
    initBalance <- getBalance srcAddr
    -- `div` 2 to leave money for tx fee
    let topBalance = unsafeGetCoin initBalance `div` 2
    coins <- pick $ map mkCoin <$> splitWord topBalance (fromIntegral destLen)
    policy <- pick arbitrary
    let newBatchP =
            NewBatchPayment
                { npbFrom = encodeCType srcAccId
                , npbTo = fromList $ zip dstCAddrs coins
                , npbInputSelectionPolicy = policy
                }
    void $ lift $ newPaymentBatch pswd newBatchP
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
    mapM_ (uncurry expectedAddrBalance) $ zip dstAddrs coins
    when (policy == OptimizeForSecurity) $
        expectedAddrBalance srcAddr (mkCoin 0)
    changeBalance <- mkCoin . fromIntegral . sumCoins <$> mapM getBalance changeAddrs
    assertProperty (changeBalance <= initBalance `unsafeSubCoin` fee) $
        "Minimal tx fee isn't satisfied"

    -- Validate that tx meta was added when transaction was processed
    forM_ (ordNub $ walId:dstWalIds) $ \wId -> do
        txMetas <- maybeStopProperty "Wallet doesn't exist" =<< lift (WS.getWalletTxHistory wId)
        void $ expectedOne "TxMeta for wallet" txMetas

    -- Validate change and used address
    -- TODO implement it when access
    -- to these addresses will be provided considering mempool
    -- expectedUserAddresses
    -- expectedChangeAddresses
  where
    getAddress srcAccId =
        lift . decodeCTypeOrFail . cwamId . adiCWAddressMeta =<<
        expectedOne "address" =<<
        lift (getAccountAddrsOrThrow WS.Existing srcAccId)
    oneNewPaymentBatchDesc =
        "Send money from one own address to multiple own addresses; " <>
        "check balances validity for destination addresses, source address and change address; " <>
        "validate history and used/change addresses"
