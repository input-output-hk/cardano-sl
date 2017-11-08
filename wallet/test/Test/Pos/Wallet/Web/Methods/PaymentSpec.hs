module Test.Pos.Wallet.Web.Methods.PaymentSpec
       ( spec
       ) where

import           Universum

import           Data.Default                   (def)
import           Data.List                      ((!!))
import           Formatting                     (build, sformat, (%))
import           Test.Hspec                     (Spec, describe)
import           Test.Hspec.QuickCheck          (modifyMaxSuccess)
import           Test.QuickCheck                (choose)
import           Test.QuickCheck.Monadic        (pick)

import           Pos.Client.Txp.Balances        (getBalance)
import           Pos.Client.Txp.Util            (txToLinearFee)
import           Pos.Core                       (TxFeePolicy (..), bvdTxFeePolicy, mkCoin,
                                                 unsafeGetCoin, unsafeSubCoin)
import           Pos.DB.Class                   (MonadGState (..))
import           Pos.Launcher                   (HasConfigurations)
import           Pos.Txp                        (Tx (..), TxAux (..), TxFee (..), _TxOut)
import           Pos.Util.CompileInfo           (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Account         (myRootAddresses)
import           Pos.Wallet.Web.ClientTypes     (CAccount (..), CWAddressMeta (..))
import           Pos.Wallet.Web.Methods.Logic   (getAccounts)
import           Pos.Wallet.Web.Methods.Payment (newPayment)
import qualified Pos.Wallet.Web.State.State     as WS
import           Pos.Wallet.Web.Util            (decodeCTypeOrFail,
                                                 getAccountAddrsOrThrow)
import           Test.Pos.Util                  (assertProperty, maybeStopProperty,
                                                 stopProperty, withDefConfigurations)

import           Test.Pos.Wallet.Web.Mode       (WalletProperty, getSentTxs,
                                                 walletPropertySpec)
import           Test.Pos.Wallet.Web.Util       (deriveRandomAddress, expectedAddrBalance,
                                                 importSomeWallets)


-- TODO remove HasCompileInfo when MonadWalletWebMode will be splitted.
spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "Wallet.Web.Methods.Payment" $ modifyMaxSuccess (const 10) $ do
    describe "newPayment" $ do
        describe "One payment" oneNewPaymentSpec

oneNewPaymentSpec :: (HasCompileInfo, HasConfigurations) => Spec
oneNewPaymentSpec = walletPropertySpec oneNewPaymentDesc $ do
    passphrases <- importSomeWallets
    (dstCAddr, dstWalId) <- deriveRandomAddress passphrases
    let l = length passphrases
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
    coins <- pick $ mkCoin <$> choose (1, unsafeGetCoin initBalance `div` 2)
    void $ lift $ newPayment pswd srcAccId dstCAddr coins def
    dstAddr <- lift $ decodeCTypeOrFail dstCAddr
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
    when (walId /= dstWalId) $ do
        txMetasSource <- maybeStopProperty "Source wallet doesn't exist" =<< lift (WS.getWalletTxHistory walId)
        void $ expectedOne "TxMeta for source wallet" txMetasSource

        txMetasDst <- maybeStopProperty "Dst wallet doesn't exist" =<< lift (WS.getWalletTxHistory dstWalId)
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
    expectedOne :: Text -> [a] -> WalletProperty a
    expectedOne what []     = stopProperty $ "expected at least one " <> what <> ", but list empty"
    expectedOne _ [x]       = pure x
    expectedOne what (_:_)  = stopProperty $ "expected one " <> what <> ", but list contains more elements"

    oneNewPaymentDesc =
        "Send money from one own address to another; " <>
        "check balances validity for destination address, source address and change address; " <>
        "validate history and used/change addresses"
