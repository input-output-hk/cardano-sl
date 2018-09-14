{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Wallet.API.WIP.LegacyHandlers (
      handlers
    ) where

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Formatting (build, sformat)
import qualified Serokell.Util.Base16 as B16
import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0 (WalletSnapshot, askWalletSnapshot)
import qualified Pos.Wallet.Web.State.Storage as V0

import           Cardano.Crypto.Wallet (xsignature)
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.WIP as WIP
import           Pos.Binary.Class (decodeFull', serialize')
import           Pos.Chain.Txp (Tx, TxAux, TxSigData, TxpConfiguration)
import           Pos.Chain.Update ()
import           Pos.Client.KeyStorage (addPublicKey)
import           Pos.Client.Txp.Util (TxError (..), defaultInputSelectionPolicy)
import qualified Pos.Core as Core
import           Pos.Crypto (PublicKey, Signature (..), hash)

import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Util (HasLens, maybeThrow)
import           Pos.Util.Servant (encodeCType)
import qualified Pos.Wallet.WalletMode as V0
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Servant

handlers :: (forall a. MonadV1 a -> Handler a)
            -> Core.Config
            -> TxpConfiguration
            -> Diffusion MonadV1
            -> Server WIP.API
handlers naturalTransformation coreConfig txpConfig diffusion =
         hoist' (Proxy @WIP.API) (handlersPlain coreConfig txpConfig submitTx)
  where
    hoist'
        :: forall (api :: *). HasServer api '[]
        => Proxy api
        -> ServerT api MonadV1
        -> Server api
    hoist' p = hoistServer p naturalTransformation
    submitTx = sendTx diffusion

-- | All the @Servant@ handlers for wallet-specific operations.
handlersPlain :: Core.Config
         -> TxpConfiguration
         -> (TxAux -> MonadV1 Bool)
         -> ServerT WIP.API MonadV1
handlersPlain coreConfig txpConfig submitTx = checkExternalWallet coreConfig
    :<|> newExternalWallet coreConfig
    :<|> deleteExternalWallet
    :<|> newUnsignedTransaction coreConfig
    :<|> newSignedTransaction coreConfig txpConfig submitTx

-- | Check if external wallet is presented in node's wallet db.
checkExternalWallet
    :: ( V0.MonadWalletLogic ctx m
       , V0.MonadWalletHistory ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => Core.Config
    -> PublicKeyAsBase58
    -> m (WalletResponse WalletAndTxHistory)
checkExternalWallet coreConfig encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    ws <- V0.askWalletSnapshot
    let walletId = makeWalletIdFrom rootPK
    walletExists <- V0.doesWalletExist walletId
    (v0wallet, transactions, isWalletReady) <- if walletExists
        then do
            -- Wallet is here, it means that user already used this wallet (for example,
            -- hardware device) on this computer, so we have to return stored information
            -- about this wallet and history of transactions (if any transactions was made).
            --
            -- By default we have to specify account and address for getting transactions
            -- history. But currently all we have is root PK, so we return complete history
            -- of transactions, for all accounts and addresses.
            let allAccounts = getWalletAccountIds ws walletId
                -- We want to get a complete history, so we shouldn't specify an address.
                address = Nothing
            (V0.WalletHistory history, _) <- V0.getHistory walletId
                                                           (const allAccounts)
                                                           address
            v1Transactions <- mapM (\(_, (v0Tx, _)) -> migrate v0Tx) $ Map.toList history
            (,,) <$> V0.getWallet walletId
                 <*> pure v1Transactions
                 <*> pure True
        else do
            -- No such wallet in db, it means that this wallet (for example, hardware
            -- device) was not used on this computer. But since this wallet _could_ be
            -- used on another computer, we have to (try to) restore this wallet.
            -- Since there's no wallet meta-data, we use default one.
            let largeCurrencyUnit = 0
                defaultMeta = V0.CWalletMeta "External wallet"
                                             V0.CWAStrict
                                             largeCurrencyUnit
                -- This is a new wallet, currently un-synchronized, so there's no
                -- history of transactions yet.
                transactions = []
            (,,) <$> restoreExternalWallet coreConfig defaultMeta encodedRootPK
                 <*> pure transactions
                 <*> pure False -- We restore wallet, so it's unready yet.

    v1wallet <- migrateWallet ws v0wallet isWalletReady
    let walletAndTxs = WalletAndTxHistory v1wallet transactions
    single <$> pure walletAndTxs

-- | Creates a new or restores an existing external @wallet@ given a 'NewExternalWallet' payload.
-- Returns to the client the representation of the created or restored wallet in the 'Wallet' type.
newExternalWallet
    :: ( MonadThrow m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       , V0.MonadBlockchainInfo m
       , V0.MonadWalletLogic ctx m
       )
    => Core.Config
    -> NewExternalWallet
    -> m (WalletResponse Wallet)
newExternalWallet coreConfig (NewExternalWallet rootPK assuranceLevel name operation) = do
    let newExternalWalletHandler CreateWallet  = createNewExternalWallet
        newExternalWalletHandler RestoreWallet = restoreExternalWallet coreConfig
    walletMeta <- V0.CWalletMeta <$> pure name
                                 <*> migrate assuranceLevel
                                 <*> pure 0
    single <$> do
        v0wallet <- newExternalWalletHandler operation walletMeta rootPK
        ws <- V0.askWalletSnapshot
        migrateWallet ws v0wallet True

-- | Creates new external wallet.
createNewExternalWallet
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CWalletMeta
    -> PublicKeyAsBase58
    -> m V0.CWallet
createNewExternalWallet walletMeta encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    -- This extended public key will be used during synchronization
    -- with the blockchain.
    addPublicKey rootPK

    let walletId = makeWalletIdFrom rootPK
        isReady  = True -- We don't need to sync new wallet with the blockchain.

    -- Create new external wallet.
    -- This is safe: if the client will try to create an external wallet from the same
    -- root public key - error will be thrown.
    void $ V0.createWalletSafe walletId walletMeta isReady

    addInitAccountInExternalWallet walletId

    V0.getWallet walletId

-- | Restore external wallet using it's root public key and metadata.
restoreExternalWallet
    :: ( MonadThrow m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       , V0.MonadWalletLogic ctx m
       )
    => Core.Config
    -> V0.CWalletMeta
    -> PublicKeyAsBase58
    -> m V0.CWallet
restoreExternalWallet coreConfig walletMeta encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    let walletId = makeWalletIdFrom rootPK
     -- Public key will be used during synchronization with the blockchain.
    addPublicKey rootPK

    let isReady = False -- Because we want to sync this wallet with the blockchain!

    -- Create new external wallet with initial account.
    void $ V0.createWalletSafe walletId walletMeta isReady
    addInitAccountInExternalWallet walletId

    -- Restoring this wallet.
    V0.restoreExternalWallet coreConfig rootPK

addInitAccountInExternalWallet
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CId V0.Wal
    -> m ()
addInitAccountInExternalWallet walletId = do
    let accountName = "Initial account"
        accountMeta = V0.CAccountMeta accountName
        accountInit = V0.CAccountInit accountMeta walletId
        includeUnready = True
    void $ V0.newExternalAccountIncludeUnready includeUnready accountInit

-- | On the disk, once imported or created, there's so far not much difference
-- between a wallet and an external wallet, except one: node stores a public key
-- for external wallet, there's no secret key.
deleteExternalWallet
    :: (V0.MonadWalletLogic ctx m)
    => PublicKeyAsBase58
    -> m NoContent
deleteExternalWallet encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK
    V0.deleteExternalWallet rootPK

migrateWallet
    :: ( V0.MonadWalletLogicRead ctx m
       , V0.MonadBlockchainInfo m
       )
    => V0.WalletSnapshot
    -> V0.CWallet
    -> Bool
    -> m Wallet
migrateWallet snapshot wallet walletIsReady = do
    let walletId = V0.cwId wallet
    walletInfo <- if walletIsReady
        then maybeThrow WalletNotFound $ V0.getWalletInfo walletId snapshot
        else
            -- Wallet is not ready yet (because of restoring),
            -- the only information we can provide is the default one.
            pure $ V0.getUnreadyWalletInfo snapshot
    walletIsExternal <- V0.isWalletExternal walletId
    let walletType = if walletIsExternal then WalletExternal else WalletRegular
    currentDepth <- V0.networkChainDifficulty
    migrate (wallet, walletInfo, walletType, currentDepth)

mkPublicKeyOrFail
    :: MonadThrow m
    => PublicKeyAsBase58
    -> m PublicKey
mkPublicKeyOrFail encodedRootPK =
    case mkPublicKeyFromBase58 encodedRootPK of
        Left problem -> throwM (InvalidPublicKey $ sformat build problem)
        Right rootPK -> return rootPK

newUnsignedTransaction
    :: forall ctx m . (V0.MonadWalletTxFull ctx m)
    => Core.Config
    -> PaymentWithChangeAddress
    -> m (WalletResponse RawTransaction)
newUnsignedTransaction coreConfig paymentWithChangeAddress = do
    -- Creating a new transaction as usual, but we don't sign or publish it.
    -- This transaction will be signed on the client-side (mobile client or
    -- hardware wallet), and after that transaction (with its signatures) will be
    -- sent to backend.
    let (PaymentWithChangeAddress pmt@Payment{..} encodedChangeAddr) = paymentWithChangeAddress
    changeAddress <- either (throwM . InvalidAddressFormat)
                            pure
                            (V1.mkAddressFromBase58 encodedChangeAddr)

    batchPayment <- createBatchPayment pmt
    (tx, srcAddressesInfo) <- V0.newUnsignedTransaction coreConfig batchPayment changeAddress
    let txAsBytes = serialize' tx
    if BS.length txAsBytes > txMaxLengthInBytes
        then throwM TooBigTransaction
        else do
            let txHash = hash tx
                txSigDataInHexFormat = mkTransactionHashAsBase16 txHash
                txInHexFormat = mkTransactionAsBase16 tx
                srcAddressesWithPaths =
                    map (\(addr, derivationPath) ->
                            AddressAndPath (mkAddressAsBase58 addr)
                                           (map word32ToAddressLevel derivationPath))
                        $ NE.toList srcAddressesInfo
                rawTx = RawTransaction txInHexFormat
                                       txSigDataInHexFormat
                                       srcAddressesWithPaths
            pure $ single rawTx
  where
    -- Max size of transaction is a mandatory limit for hardware wallets like Ledger Nano S.
    txMaxLengthInBytes = 4096

-- | It is assumed that we received a transaction which was signed
-- on the client side (mobile client or hardware wallet).
-- Now we have to submit this transaction as usually.
newSignedTransaction
    :: forall ctx m . (V0.MonadWalletTxFull ctx m)
    => Core.Config
    -> TxpConfiguration
    -> (TxAux -> m Bool)
    -> SignedTransaction
    -> m (WalletResponse Transaction)
newSignedTransaction coreConfig
                     txpConfig
                     submitTx
                     (SignedTransaction encodedRootPK encodedTx encodedAddrsWithProofs) = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    let walletId = makeWalletIdFrom rootPK
        txAsHex = rawTransactionAsBase16 encodedTx
    txRaw <- case B16.decode txAsHex of
        Left _      -> throwM SignedTxNotBase16Format
        Right txRaw -> return txRaw

    tx <- case decodeFull' txRaw of
        Left problem     -> throwM (SignedTxUnableToDecode $ toText problem)
        Right (tx :: Tx) -> return tx

    addrsWithProofs <- mapM checkAddressProofValidity encodedAddrsWithProofs

    -- Submit signed transaction as pending one.
    cTx <- V0.submitSignedTransaction coreConfig txpConfig submitTx walletId tx addrsWithProofs
    single <$> migrate cTx
  where
    -- Check validity of the source address and the proof that we have a right
    -- to spend money from corresponding input. Please note that it only checks
    -- common validity (like Base16-format), crypto-validity will be checked later.
    checkAddressProofValidity
        :: AddressWithProof
        -> m (Address, Signature TxSigData, PublicKey)
    checkAddressProofValidity (AddressWithProof encodedAddr encodedTxSig encodedDerivedPK) = do
        srcAddress <- either (throwM . InvalidAddressFormat)
                             pure
                             (V1.mkAddressFromBase58 encodedAddr)

        txSignature <- case B16.decode (rawTransactionSignatureAsBase16 encodedTxSig) of
            Left _ -> throwM SignedTxSignatureNotBase16Format
            Right txSigItself -> case xsignature txSigItself of
                Left problem -> throwM (SignedTxInvalidSignature $ toText problem)
                Right realTxSig -> return (Signature realTxSig :: Signature TxSigData)

        derivedPK <- case mkPublicKeyFromBase58 encodedDerivedPK of
            Left problem -> throwM (InvalidPublicKey $ sformat build problem)
            Right derivedPK -> return derivedPK

        return (srcAddress, txSignature, derivedPK)

-- | Helper function to reduce code duplication
createBatchPayment
    :: forall ctx m . (V0.MonadWalletTxFull ctx m)
    => Payment
    -> m (V0.NewBatchPayment)
createBatchPayment (Payment source destinations groupingPolicy _) = do
    cAccountId <- migrate source
    addrCoinList <- migrate $ NE.toList destinations
    let (V1 policy) = fromMaybe (V1 defaultInputSelectionPolicy) groupingPolicy
    return $ V0.NewBatchPayment cAccountId addrCoinList policy

makeWalletIdFrom :: PublicKey -> V0.CId V0.Wal
makeWalletIdFrom = encodeCType . Core.makePubKeyAddressBoot
