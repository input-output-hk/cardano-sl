module Test.Pos.Wallet.Web.Methods.PaymentSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Data.List ((!!))
import           Formatting (build, sformat, (%))
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (choose)
import           Test.QuickCheck.Monadic (pick)

import           Pos.Client.Txp.Balances (getBalance)
import           Pos.Client.Txp.Util (txToLinearFee)
import           Pos.Core (TxFeePolicy (..), bvdTxFeePolicy, mkCoin, unsafeGetCoin, unsafeSubCoin)
import           Pos.Core.Txp (Tx (..), TxAux (..), _TxOut)
import           Pos.DB.Class (MonadGState (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Txp (TxFee (..))
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Account (myRootAddresses)
import           Pos.Wallet.Web.ClientTypes (CAccount (..), CWAddressMeta (..))
import           Pos.Wallet.Web.Methods.Logic (getAccounts)
import           Pos.Wallet.Web.Methods.Payment (newPayment)
import qualified Pos.Wallet.Web.State.State as WS
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getAccountAddrsOrThrow)
import           Test.Pos.Util (assertProperty, expectedOne, maybeStopProperty, stopProperty,
                                withDefConfigurations)

import           Test.Pos.Wallet.Web.Mode (getSentTxs, walletPropertySpec)
import           Test.Pos.Wallet.Web.Util (deriveRandomAddress, expectedAddrBalance,
                                           importSomeWallets, mostlyEmptyPassphrases)


-- TODO remove HasCompileInfo when MonadWalletWebMode is split.
spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "Wallet.Web.Methods.Payment" $ modifyMaxSuccess (const 10) $ do
    describe "newPayment" $ do
        describe "One payment" oneNewPaymentSpec

oneNewPaymentSpec :: (HasCompileInfo, HasConfigurations) => Spec
oneNewPaymentSpec = walletPropertySpec oneNewPaymentDesc $ do
    -- Addresses of secret wallets
    passphrases <- importSomeWallets mostlyEmptyPassphrases
    let l = length passphrases
    rootWalletIds <- lift myRootAddresses
    idx <- pick $ choose (0, l - 1)
    let rootWalletId = rootWalletIds !! idx
    let passphrase = passphrases !! idx
    let accountUndefined = sformat ("There is no account defined for wallet index: "%build) rootWalletId
    srcAccount <- maybeStopProperty accountUndefined =<< (lift $ head <$> getAccounts (Just rootWalletId))
    srcAccId <- lift $ decodeCTypeOrFail (caId srcAccount)
    srcAddr <- getAddress srcAccId

    -- A random destination address / wallet id
    (destClientAddress, destWalletId) <- deriveRandomAddress passphrases
    initBalance <- getBalance srcAddr

    -- `div` 2 to leave money for tx fee
    coins <- pick $ mkCoin <$> choose (1, unsafeGetCoin initBalance `div` 2)
    void $ lift $ newPayment passphrase srcAccId destClientAddress coins def
    dstAddr <- lift $ decodeCTypeOrFail destClientAddress

    txLinearPolicy <- lift $ (bvdTxFeePolicy <$> gsAdoptedBVData) <&> \case
        TxFeePolicyTxSizeLinear linear -> linear
        _                              -> error "unknown fee policy"
    txAux <- expectedOne "sent TxAux" =<< lift getSentTxs
    TxFee fee <- lift (runExceptT $ txToLinearFee txLinearPolicy txAux) >>= \case
        Left er -> stopProperty $ "Couldn't compute tx fee by tx, reason: " <> pretty er
        Right x -> pure x
    let outAddresses = map (fst . view _TxOut) $ toList $ _txOutputs $ taTx txAux
    changeAddr <- expectedOne "expected one change address" (filter (/= dstAddr) outAddresses)

    -- Validate balances
    expectedAddrBalance dstAddr coins
    expectedAddrBalance srcAddr (mkCoin 0)
    changeBalance <- getBalance changeAddr
    assertProperty (changeBalance <= initBalance `unsafeSubCoin` fee) $
        "Minimal tx fee isn't satisfied"

    -- Validate that tx meta was added when transaction was processed
    when (rootWalletId /= destWalletId) $ do
        txMetasSource <- maybeStopProperty "Source wallet doesn't exist" =<< lift (WS.getWalletTxHistory rootWalletId)
        void $ expectedOne "TxMeta for source wallet" txMetasSource

        txMetasDst <- maybeStopProperty "Destination wallet doesn't exist" =<< lift (WS.getWalletTxHistory destWalletId)
        void $ expectedOne "TxMeta for dst wallet" txMetasDst

    -- Validate change and used address
    -- TODO implement it when access
    -- to these addresses will be provided considering mempool
    -- expectedUserAddresses
    -- expectedChangeAddresses
  where
    getAddress srcAccId =
        lift . decodeCTypeOrFail . cwamId =<<
        expectedOne "address" =<<
        lift (getAccountAddrsOrThrow WS.Existing srcAccId)
    oneNewPaymentDesc =
        "Sends money from one address to another; " <>
        "Checks balance validity on the destination address, source address and change address; " <>
        "Validates the history and used / change addresses"
