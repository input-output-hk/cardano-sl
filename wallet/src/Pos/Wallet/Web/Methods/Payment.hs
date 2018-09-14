{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction creation and fees

module Pos.Wallet.Web.Methods.Payment
       ( MoneySource(..)
       , MonadFees
       , getMoneySourceUtxo
       , newPayment
       , newPaymentBatch
       , newUnsignedTransaction
       , submitSignedTransaction
       , getTxFee
       ) where

import           Universum

import           Control.Monad.Except (runExcept)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Time.Units (Second)
import qualified Data.Vector as V
import           Formatting (build, sformat, (%))
import           Servant.Server (err403, err405, errReasonPhrase)
import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Txp (Tx, TxAux (..), TxFee (..), TxIn (..),
                     TxInWitness (..), TxOut (..), TxOutAux (..), TxSigData,
                     TxpConfiguration, Utxo, _txInputs, _txOutputs)
import           Pos.Client.KeyStorage (getSecretKeys)
import           Pos.Client.Txp.Addresses (MonadAddresses)
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Client.Txp.Network (prepareMTx, prepareUnsignedTx)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), computeTxFee,
                     runTxCreator)
import           Pos.Configuration (walletTxCreationDisabled)
import           Pos.Core as Core (Address, Coin, Config (..),
                     getCurrentTimestamp)
import           Pos.Core.Conc (concurrently, delay)
import           Pos.Core.Genesis (GenesisData)
import           Pos.Crypto (PassPhrase, PublicKey, SafeSigner,
                     ShouldCheckPassphrase (..), Signature, checkPassMatches,
                     hash, withSafeSignerUnsafe)
import           Pos.DB (MonadGState)
import           Pos.Util (eitherToThrow, maybeThrow)
import           Pos.Util.Servant (encodeCType)
import           Pos.Util.Wlog (logDebug)
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.WalletBackup ()
import           Pos.Wallet.Web.Account (getSKByAddressPure, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CCoin, CId,
                     CTx (..), NewBatchPayment (..), Wal)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTxMeta, constructCTx,
                     getCurChainDifficulty)
import           Pos.Wallet.Web.Methods.Txp (MonadWalletTxFull,
                     coinDistrToOutputs, getPendingAddresses, rewrapTxError,
                     submitAndSaveNewPtx)
import           Pos.Wallet.Web.Pending (mkPendingTx)
import           Pos.Wallet.Web.State (AddressInfo (..),
                     AddressLookupMode (Ever, Existing), HasWAddressMeta (..),
                     WAddressMeta (..), WalletDbReader, WalletSnapshot,
                     askWalletDB, askWalletSnapshot, getWalletSnapshot,
                     isWalletRestoring, wamAccount)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getAccountAddrsOrThrow,
                     getWalletAccountIds, getWalletAddrsDetector)

newPayment
    :: MonadWalletTxFull ctx m
    => Core.Config
    -> TxpConfiguration
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> InputSelectionPolicy
    -> m CTx
newPayment coreConfig txpConfig submitTx passphrase srcAccount dstAddress coin policy =
    -- This is done for two reasons:
    -- 1. In order not to overflow relay.
    -- 2. To let other things (e. g. block processing) happen if
    -- `newPayment`s are done continuously.
    notFasterThan (6 :: Second) $ do
      sendMoney
          coreConfig
          txpConfig
          submitTx
          passphrase
          (AccountMoneySource srcAccount)
          (one (dstAddress, coin))
          policy

newPaymentBatch
    :: MonadWalletTxFull ctx m
    => Core.Config
    -> TxpConfiguration
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> NewBatchPayment
    -> m CTx
newPaymentBatch coreConfig txpConfig submitTx passphrase NewBatchPayment {..} = do
    src <- decodeCTypeOrFail npbFrom
    notFasterThan (6 :: Second) $ do
      sendMoney
          coreConfig
          txpConfig
          submitTx
          passphrase
          (AccountMoneySource src)
          npbTo
          npbInputSelectionPolicy

newUnsignedTransaction
    :: MonadWalletTxFull ctx m
    => Core.Config
    -> NewBatchPayment
    -> Address
    -> m (Tx, NonEmpty (Address, [Word32]))
newUnsignedTransaction coreConfig NewBatchPayment {..} changeAddress = do
    src <- decodeCTypeOrFail npbFrom
    createNewUnsignedTransaction
        coreConfig
        (AccountMoneySource src)
        npbTo
        npbInputSelectionPolicy
        changeAddress

-- | Submit externally-signed transaction to the blockchain.
submitSignedTransaction
    :: MonadWalletTxFull ctx m
    => Core.Config
    -> TxpConfiguration
    -> (TxAux -> m Bool)
    -> CId Wal
    -> Tx
    -> [(Address, Signature TxSigData, PublicKey)]
    -> m CTx
submitSignedTransaction coreConfig
                        txpConfig
                        submitTx
                        srcWalletId
                        tx
                        srcAddrsWithProofs = do
    when walletTxCreationDisabled $
        throwM err405
        { errReasonPhrase = "Transaction creation (including externally-signed one) is disabled by configuration!"
        }

    db <- askWalletDB
    ws <- getWalletSnapshot db

    when (isWalletRestoring ws srcWalletId) $
        throwM err403
        { errReasonPhrase = "Transaction creation is disabled when the wallet is restoring."
        }

    let moneySource = WalletMoneySource srcWalletId
    addrMetas' <- getMoneySourceAddresses ws moneySource
    _ <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Given money source has no addresses!")

    let srcAddrs = map (\(addr, _, _) -> addr) srcAddrsWithProofs
    utxoForSrcAddrs <- getOwnUtxos (configGenesisData coreConfig) srcAddrs

    th <- rewrapTxError "Cannot send externally-signed transaction" $ do
        let outputs = toList $ _txOutputs tx
            inputs  = toList $ _txInputs tx

        witnesses <- mapM (makeWitness utxoForSrcAddrs) inputs
        let txAux = TxAux tx $ V.fromList witnesses

        ts <- Just <$> getCurrentTimestamp
        let dstAddrs = map txOutAddress outputs
            -- Technically, we don't need a hash because we already have
            -- a tx signature (from external wallet), but 'THEntry' requires a hash.
            txHash   = hash tx
            th       = THEntry txHash tx Nothing outputs dstAddrs ts

        ptx <- mkPendingTx (configProtocolConstants coreConfig)
                           ws
                           srcWalletId
                           txHash
                           txAux
                           th



        th <$ submitAndSaveNewPtx coreConfig txpConfig db submitTx ptx

    -- We add TxHistoryEntry's meta created by us in advance
    -- to make TxHistoryEntry in CTx consistent with entry in history.
    _ <- addHistoryTxMeta db srcWalletId th
    ws' <- getWalletSnapshot db
    let srcWalletAddrsDetector = getWalletAddrsDetector ws' Ever srcWalletId

    diff <- getCurChainDifficulty
    fst <$> constructCTx ws' srcWalletId srcWalletAddrsDetector diff th
  where
    -- 'PkWitness' contains:
    -- 1. a signature of entire transaction,
    -- 2. derived PK.
    -- Since this 'TxIn' corresponds to some output (which contains address `A`),
    -- derived PK is a key address `A` was generated from. This is our proof that
    -- we have a right to spend money from the `A`.
    makeWitness
        :: MonadWalletTxFull ctx m
        => Utxo
        -> TxIn
        -> m TxInWitness
    makeWitness _ (TxInUnknown w bs) =
        return $ UnknownWitnessType w bs
    makeWitness ownUtxo txIn =
        case M.lookup txIn ownUtxo of
            Nothing -> throwM $ RequestError "makeWitness: cannot find input in ownUtxo"
            Just (TxOutAux txOut) -> do
                let srcAddrWithProof = find (\(srcAddress, _, _) -> srcAddress == txOutAddress txOut)
                                            srcAddrsWithProofs
                case srcAddrWithProof of
                    Nothing -> throwM $ RequestError "makeWitness: cannot find src address in proofs"
                    Just (_, txSignature, derivedPK) ->
                        return $ PkWitness derivedPK txSignature

-- | Create new raw transaction which will be signed externally (for example, by Ledger device).
createNewUnsignedTransaction
    :: (MonadWalletTxFull ctx m)
    => Core.Config
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> InputSelectionPolicy
    -> Address
    -> m (Tx, NonEmpty (Address, [Word32]))
createNewUnsignedTransaction coreConfig moneySource dstDistr policy changeAddress = do
    when walletTxCreationDisabled $
        throwM err405
        { errReasonPhrase = "Transaction creation (including unsigned ones) is disabled by configuration!"
        }

    db <- askWalletDB
    ws <- getWalletSnapshot db

    addrMetas' <- getMoneySourceAddresses ws moneySource
    addrMetas <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Unsigned transaction: Given money source has no addresses!")

    let srcAddrs = map (view wamAddress) addrMetas
        srcAddrsDerivationPaths = map (\meta -> [_wamAccountIndex meta, _wamAddressIndex meta]) addrMetas
        srcAddrsInfo = NE.zip srcAddrs srcAddrsDerivationPaths

    logDebug "createNewUnsignedTransaction: processed addrs"

    _ <- getSomeMoneySourceAccount ws moneySource
    outputs <- coinDistrToOutputs dstDistr
    let pendingAddrs = getPendingAddresses ws policy
    rewrapTxError "Cannot create unsigned transaction" $
        prepareUnsignedTx coreConfig pendingAddrs policy srcAddrs outputs changeAddress >>= \case
            Left txError ->
                throwM (RequestError $ show txError)
            Right (tx, _) ->
                return (tx, srcAddrsInfo)

type MonadFees ctx m =
    ( MonadCatch m
    , MonadGState m
    , WalletDbReader ctx m
    , MonadAddresses m
    , MonadBalances m
    , MonadIO m
    )

getTxFee
     :: MonadFees ctx m
     => Core.Config
     -> AccountId
     -> CId Addr
     -> Coin
     -> InputSelectionPolicy
     -> m CCoin
getTxFee coreConfig srcAccount dstAccount coin policy = do
    ws <- askWalletSnapshot
    let pendingAddrs = getPendingAddresses ws policy
    utxo <- getMoneySourceUtxo (configGenesisData coreConfig)
                               ws
                               (AccountMoneySource srcAccount)
    outputs <- coinDistrToOutputs $ one (dstAccount, coin)
    TxFee fee <- rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< runTxCreator policy (computeTxFee coreConfig pendingAddrs utxo outputs)
    pure $ encodeCType fee

data MoneySource
    = WalletMoneySource (CId Wal)
    | AccountMoneySource AccountId
    | AddressMoneySource WAddressMeta
    deriving (Show, Eq)

getMoneySourceAddresses :: MonadThrow m
                        => WalletSnapshot
                        -> MoneySource
                        -> m [WAddressMeta]
getMoneySourceAddresses _ (AddressMoneySource addrId) = return $ one addrId
getMoneySourceAddresses ws (AccountMoneySource accId) =
    map adiWAddressMeta <$> getAccountAddrsOrThrow ws Existing accId
getMoneySourceAddresses ws (WalletMoneySource wid) =
    concatMapM (getMoneySourceAddresses ws . AccountMoneySource)
               (getWalletAccountIds ws wid)

getSomeMoneySourceAccount :: MonadThrow m
                          => WalletSnapshot -> MoneySource -> m AccountId
getSomeMoneySourceAccount _ (AddressMoneySource addrId) =
    return $ addrId ^. wamAccount
getSomeMoneySourceAccount _ (AccountMoneySource accId) = return accId
getSomeMoneySourceAccount ws (WalletMoneySource wid) = do
    wAddr <- maybeThrow noWallets ((fmap fst . uncons) (getWalletAccountIds ws wid))
    getSomeMoneySourceAccount ws (AccountMoneySource wAddr)
  where
    noWallets = InternalError "Wallet has no accounts"

getMoneySourceWallet :: MoneySource -> CId Wal
getMoneySourceWallet (AddressMoneySource addrId) = addrId ^. wamWalletId
getMoneySourceWallet (AccountMoneySource accId)  = aiWId accId
getMoneySourceWallet (WalletMoneySource wid)     = wid

getMoneySourceUtxo :: (MonadThrow m, MonadBalances m)
                   => GenesisData
                   -> WalletSnapshot
                   -> MoneySource
                   -> m Utxo
getMoneySourceUtxo genesisData ws =
    getMoneySourceAddresses ws >=>
    mapM (return . view wamAddress) >=>
    getOwnUtxos genesisData

sendMoney
    :: (MonadWalletTxFull ctx m)
    => Core.Config
    -> TxpConfiguration
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> InputSelectionPolicy
    -> m CTx
sendMoney coreConfig txpConfig submitTx passphrase moneySource dstDistr policy = do
    db <- askWalletDB
    ws <- getWalletSnapshot db
    when walletTxCreationDisabled $
        throwM err405
        { errReasonPhrase = "Transaction creation is disabled by configuration!"
        }
    let srcWallet = getMoneySourceWallet moneySource
    when (isWalletRestoring ws srcWallet) $
        throwM err403
        { errReasonPhrase = "Transaction creation is disabled when the wallet is restoring."
        }
    rootSk <- maybe (throwM (RequestError $ sformat ("No regular source wallet with address "%build)
                                                    srcWallet))
                    pure
                    =<< getSKById srcWallet

    checkPassMatches passphrase rootSk `whenNothing`
        throwM (RequestError "Passphrase doesn't match")

    addrMetas' <- getMoneySourceAddresses ws moneySource
    addrMetas <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Given money source has no addresses!")

    let srcAddrs = map (view wamAddress) addrMetas

    logDebug "sendMoney: processed addrs"

    let metasAndAddresses = M.fromList $ zip (toList srcAddrs) (toList addrMetas)
    allSecrets <- getSecretKeys

    let
        getSigner :: Address -> Maybe SafeSigner
        getSigner addr = do
          addrMeta <- M.lookup addr metasAndAddresses
          sk <- rightToMaybe . runExcept $
              getSKByAddressPure allSecrets (ShouldCheckPassphrase False) passphrase addrMeta
          withSafeSignerUnsafe sk (pure passphrase) pure

    relatedAccount <- getSomeMoneySourceAccount ws moneySource
    outputs <- coinDistrToOutputs dstDistr
    let pendingAddrs = getPendingAddresses ws policy
    th <- rewrapTxError "Cannot send transaction" $ do
        (txAux, inpTxOuts') <-
            prepareMTx coreConfig getSigner pendingAddrs policy srcAddrs outputs (relatedAccount, passphrase)

        ts <- Just <$> getCurrentTimestamp
        let tx = taTx txAux
            txHash = hash tx
            inpTxOuts = toList inpTxOuts'
            dstAddrs  = map txOutAddress . toList $
                        _txOutputs tx
            th = THEntry txHash tx Nothing inpTxOuts dstAddrs ts
        ptx <- mkPendingTx (configProtocolConstants coreConfig)
                           ws
                           srcWallet
                           txHash
                           txAux
                           th

        th <$ submitAndSaveNewPtx coreConfig txpConfig db submitTx ptx

    -- We add TxHistoryEntry's meta created by us in advance
    -- to make TxHistoryEntry in CTx consistent with entry in history.
    _ <- addHistoryTxMeta db srcWallet th
    diff <- getCurChainDifficulty
    ws' <- getWalletSnapshot db
    let srcWalletAddrsDetector = getWalletAddrsDetector ws' Ever srcWallet

    logDebug "sendMoney: constructing response"
    fst <$> constructCTx ws' srcWallet srcWalletAddrsDetector diff th

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

notFasterThan ::
       (MonadIO m, MonadUnliftIO m) => Second -> m a -> m a
notFasterThan time action = fst <$> concurrently action (delay time)
