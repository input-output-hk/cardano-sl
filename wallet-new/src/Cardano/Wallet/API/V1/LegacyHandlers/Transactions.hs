module Cardano.Wallet.API.V1.LegacyHandlers.Transactions where

import           Universum
import qualified Serokell.Util.Base16 as B16
import           Formatting (build, sformat)

import           Cardano.Crypto.Wallet (xsignature)
import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration (HasConfigurations, MonadV1,
                                                  migrate)
import qualified Cardano.Wallet.API.V1.Transactions as Transactions
import           Cardano.Wallet.API.V1.Types
import qualified Data.IxSet.Typed as IxSet
import qualified Data.List.NonEmpty as NE
import           Pos.Binary.Class (decodeFull', serialize')
import           Pos.Client.Txp.Util (defaultInputSelectionPolicy)
import qualified Pos.Client.Txp.Util as V0
import           Pos.Core (TxAux, TxSigData, Tx)
import qualified Pos.Core as Core
import           Pos.Crypto (Signature (..), decodeBase58PublicKey)
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.History as V0
import qualified Pos.Wallet.Web.Methods.Payment as V0
import qualified Pos.Wallet.Web.Methods.Txp as V0
import qualified Pos.Wallet.Web.State as V0
import qualified Pos.Wallet.Web.Util as V0

import           Servant

handlers :: HasConfigurations
         => (TxAux -> MonadV1 Bool) -> ServerT Transactions.API MonadV1

handlers submitTx =
             newTransaction submitTx
        :<|> allTransactions
        :<|> estimateFees
        :<|> newUnsignedTransaction
        :<|> newSignedTransaction submitTx

newTransaction
    :: forall ctx m . (V0.MonadWalletTxFull ctx m)
    => (TxAux -> m Bool) -> Payment -> m (WalletResponse Transaction)
newTransaction submitTx pmt@Payment {..} = do
    let (V1 spendingPw) = fromMaybe (V1 mempty) pmtSpendingPassword
    batchPayment <- createBatchPayment pmt
    cTx <- V0.newPaymentBatch submitTx spendingPw batchPayment
    single <$> migrate cTx


allTransactions
    :: forall ctx m. (V0.MonadWalletHistory ctx m)
    => Maybe WalletId
    -> Maybe AccountIndex
    -> Maybe (V1 Core.Address)
    -> RequestParams
    -> FilterOperations Transaction
    -> SortOperations Transaction
    -> m (WalletResponse [Transaction])
allTransactions mwalletId mAccIdx mAddr requestParams fops sops  =
    case mwalletId of
        Just walletId -> do
            cIdWallet <- migrate walletId
            ws <- V0.askWalletSnapshot

            -- Create a `[V0.AccountId]` to get txs from it
            let accIds = case mAccIdx of
                    Just accIdx -> migrate (walletId, accIdx)
                    -- ^ Migrate `V1.AccountId` into `V0.AccountId` and put it into a list
                    Nothing     -> V0.getWalletAccountIds ws cIdWallet
                    -- ^ Or get all `V0.AccountId`s of a wallet

            let v0Addr = case mAddr of
                    Nothing        -> Nothing
                    Just (V1 addr) -> Just $ V0.encodeCType addr

            -- get all `[Transaction]`'s
            let transactions = do
                    (V0.WalletHistory wh, _) <- V0.getHistory cIdWallet (const accIds) v0Addr
                    migrate wh

            -- Paginate result
            respondWith requestParams fops sops (IxSet.fromList <$> transactions)
        _ ->
            -- TODO: should we use the 'FilterBy' machinery instead? that
            --       let us express RANGE, GT, etc. in addition to EQ. does
            --       that make sense for this dataset?
            throwM MissingRequiredParams
                { requiredParams = pure ("wallet_id", "WalletId")
                }

estimateFees :: (MonadThrow m, V0.MonadFees ctx m)
    => Payment
    -> m (WalletResponse EstimatedFees)
estimateFees Payment{..} = do
    ws <- V0.askWalletSnapshot
    let (V1 policy) = fromMaybe (V1 defaultInputSelectionPolicy) pmtGroupingPolicy
        pendingAddrs = V0.getPendingAddresses ws policy
    cAccountId <- migrate pmtSource
    utxo <- V0.getMoneySourceUtxo ws (V0.AccountMoneySource cAccountId)
    outputs <- V0.coinDistrToOutputs =<< mapM migrate pmtDestinations
    efee <- V0.runTxCreator policy (V0.computeTxFee pendingAddrs utxo outputs)
    case efee of
        Right fee ->
            single <$> migrate fee
        Left err ->
            throwM (convertTxError err)

newUnsignedTransaction
    :: forall ctx m . (V0.MonadWalletTxFull ctx m)
    => Payment
    -> m (WalletResponse RawTransaction)
newUnsignedTransaction pmt@Payment {..} = do
    -- We're creating new transaction as usually, but we mustn't sign/publish it.
    -- This transaction will be signed on the client-side (mobile client or
    -- hardware wallet), and after that transaction (with its signature) will be
    -- sent to backend.
    batchPayment <- createBatchPayment pmt
    tx <- V0.newUnsignedTransaction batchPayment
    let txInHexFormat = B16.encode $ serialize' tx
        rawTx = RawTransaction txInHexFormat
    pure $ single rawTx


-- | It is assumed that we received a transaction which was signed
-- on the client side (mobile client or hardware wallet).
-- Now we have to submit this transaction as usually.
newSignedTransaction
    :: forall ctx m . (V0.MonadWalletTxFull ctx m)
    => (TxAux -> m Bool)
    -> SignedTransaction
    -> m (WalletResponse Transaction)
newSignedTransaction submitTx (SignedTransaction encodedExtPubKey txAsHex signatureAsHex) = do
    publicKey <- case decodeBase58PublicKey encodedExtPubKey of
        Left problem -> throwM (InvalidPublicKey $ sformat build problem)
        Right publicKey -> return publicKey

    let walletId = V0.encodeCType . Core.makePubKeyAddressBoot $ publicKey

    txRaw <- case B16.decode txAsHex of
        Left _ -> throwM . convertTxError $ V0.SignedTxNotBase16Format
        Right txRaw -> return txRaw

    tx <- case decodeFull' txRaw of
        Left problem -> throwM . convertTxError $ (V0.SignedTxUnableToDecode $ toText problem)
        Right (tx :: Tx) -> return tx

    signature <- case B16.decode signatureAsHex of
        Left _ -> throwM . convertTxError $ V0.SignedTxSignatureNotBase16Format
        Right signatureItself ->
            case xsignature signatureItself of
                Left problem -> throwM . convertTxError $ (V0.SignedTxInvalidSignature $ toText problem)
                Right realSignature -> do
                    let signature :: Signature TxSigData
                        signature = Signature realSignature
                    return signature

    -- Submit signed transaction as pending one.
    cTx <- V0.submitSignedTransaction submitTx publicKey walletId tx signature
    single <$> migrate cTx

-- | helper function to reduce code duplication
createBatchPayment
    :: forall ctx m . (V0.MonadWalletTxFull ctx m)
    => Payment
    -> m (V0.NewBatchPayment)
createBatchPayment Payment {..} = do
    cAccountId <- migrate pmtSource
    addrCoinList <- migrate $ NE.toList pmtDestinations
    let (V1 policy) = fromMaybe (V1 defaultInputSelectionPolicy) pmtGroupingPolicy
    return (V0.NewBatchPayment cAccountId addrCoinList policy)
