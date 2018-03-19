{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction creation and fees

module Pos.Wallet.Web.Methods.Payment
       ( newPayment
       , newPaymentBatch
       , getTxFee
         -- * Required for dbgen and maybe other internal usage
       , unsafeNewPayment
       ) where

import           Universum

import           Control.Exception                (throw)
import           Control.Monad.Except             (runExcept)
import qualified Data.Map                         as M
import           Data.Time.Units                  (Second)
import           Formatting                       (sformat, (%))
import qualified Formatting                       as F
import           Mockable                         (concurrently, delay)
import           Servant.Server                   (err405, errReasonPhrase)
import           System.Wlog                      (logDebug)

import           Pos.Aeson.ClientTypes            ()
import           Pos.Aeson.WalletBackup           ()
import           Pos.Client.Txp.Addresses         (MonadAddresses (..))
import           Pos.Client.Txp.Balances          (getOwnUtxos)
import           Pos.Client.Txp.History           (TxHistoryEntry (..))
import           Pos.Client.Txp.Util              (InputSelectionPolicy (..),
                                                   computeTxFee, runTxCreator)
import           Pos.Communication                (SendActions (..), prepareMTx)
import           Pos.Configuration                (HasNodeConfiguration,
                                                   walletTxCreationDisabled)
import           Pos.Core                         (Coin, HasConfiguration, addressF,
                                                   getCurrentTimestamp)
import           Pos.Crypto                       (PassPhrase, ShouldCheckPassphrase (..),
                                                   checkPassMatches, hash,
                                                   withSafeSignerUnsafe)
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Txp                          (TxFee (..), Utxo, UtxoModifier,
                                                   getUtxoModifier, withTxpLocalData,
                                                   _txOutputs)
import           Pos.Txp.Core                     (TxAux (..), TxOut (..))
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Util                         (eitherToThrow, maybeThrow)
import           Pos.Util.LogSafe                 (logInfoS)
import           Pos.Wallet.KeyStorage            (getSecretKeys)
import           Pos.Wallet.Web.Account           (GenSeed (..), getSKByAddressPure,
                                                   getSKById)
import           Pos.Wallet.Web.ClientTypes       (AccountId (..), Addr, CCoin, CId,
                                                   CTx (..), NewBatchPayment (..), Wal,
                                                   mkCCoin)
import           Pos.Wallet.Web.Error             (WalletError (..))
import           Pos.Wallet.Web.Methods.History   (addHistoryTx, constructCTx,
                                                   getCurChainDifficulty)
import qualified Pos.Wallet.Web.Methods.Logic     as L
import           Pos.Wallet.Web.Methods.Txp       (coinDistrToOutputs,
                                                   getPendingAddresses, rewrapTxError,
                                                   submitAndSaveNewPtx)
import           Pos.Wallet.Web.Mode              (MonadWalletWebMode, WalletWebMode)
import           Pos.Wallet.Web.Pending           (mkPendingTx)
import           Pos.Wallet.Web.State             (AddressInfo (..),
                                                   AddressLookupMode (Ever, Existing),
                                                   WAddressMeta, WalletSnapshot,
                                                   askWalletDB, askWalletSnapshot,
                                                   getWalletSnapshot, wamAccount,
                                                   wamAddress, wamWalletId)
import           Pos.Wallet.Web.Util              (decodeCTypeOrFail,
                                                   getAccountAddrsOrThrow,
                                                   getWalletAccountIds,
                                                   getWalletAddrsDetector)


-- Required for non-blocking internal usage in dbgen. DO NOT USE!
unsafeNewPayment
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> InputSelectionPolicy
    -> m CTx
unsafeNewPayment sa passphrase srcAccount dstAccount coin policy =
    sendMoney
        sa
        passphrase
        (AccountMoneySource srcAccount)
        (one (dstAccount, coin))
        policy

newPayment
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> InputSelectionPolicy
    -> m CTx
newPayment sa passphrase srcAccount dstAccount coin policy =
    -- This is done for two reasons:
    -- 1. In order not to overflow relay.
    -- 2. To let other things (e. g. block processing) happen if
    -- `newPayment`s are done continuously.
    notFasterThan (6 :: Second) $ do
      sendMoney
          sa
          passphrase
          (AccountMoneySource srcAccount)
          (one (dstAccount, coin))
          policy

newPaymentBatch
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> NewBatchPayment
    -> m CTx
newPaymentBatch sa passphrase NewBatchPayment {..} = do
    src <- decodeCTypeOrFail npbFrom
    notFasterThan (6 :: Second) $ do
      sendMoney
        sa
        passphrase
        (AccountMoneySource src)
        npbTo
        npbPolicy

getTxFee
     :: MonadWalletWebMode m
     => AccountId
     -> CId Addr
     -> Coin
     -> InputSelectionPolicy
     -> m CCoin
getTxFee srcAccount dstAccount coin policy = do
    ws <- askWalletSnapshot
    updates <- withTxpLocalData getUtxoModifier
    let pendingAddrs = getPendingAddresses ws policy
    utxo <- getMoneySourceUtxo ws updates (AccountMoneySource srcAccount)
    outputs <- coinDistrToOutputs $ one (dstAccount, coin)
    TxFee fee <- rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< runTxCreator policy (computeTxFee pendingAddrs utxo outputs)
    pure $ mkCCoin fee

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
    wAddr <- maybeThrow noWallets (head (getWalletAccountIds ws wid))
    getSomeMoneySourceAccount ws (AccountMoneySource wAddr)
  where
    noWallets = InternalError "Wallet has no accounts"

getMoneySourceWallet :: MoneySource -> CId Wal
getMoneySourceWallet (AddressMoneySource addrId) = addrId ^. wamWalletId
getMoneySourceWallet (AccountMoneySource accId)  = aiWId accId
getMoneySourceWallet (WalletMoneySource wid)     = wid

getMoneySourceUtxo :: MonadWalletWebMode m
                   => WalletSnapshot
                   -> UtxoModifier
                   -> MoneySource
                   -> m Utxo
getMoneySourceUtxo ws updates =
    getMoneySourceAddresses ws >=>
    mapM (return . view wamAddress) >=>
    getOwnUtxos ws updates

-- [CSM-407] It should be moved to `Pos.Wallet.Web.Mode`, but
-- to make it possible all this mess should be neatly separated
-- to modules and refactored
instance
    ( HasConfiguration
    , HasNodeConfiguration
    , HasInfraConfiguration
    , HasGtConfiguration
    , HasUpdateConfiguration
    )
    => MonadAddresses Pos.Wallet.Web.Mode.WalletWebMode
  where
    type AddrData Pos.Wallet.Web.Mode.WalletWebMode = (AccountId, PassPhrase)
    getNewAddress (accId, passphrase) = do
        -- TODO(adinapoli) This looks like a code smell, can we
        -- remove this typeclass entirely?
        ws <- askWalletSnapshot
        wam <- L.newAddress_ ws RandomSeed passphrase accId
        return $ wam ^. wamAddress

sendMoney
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> InputSelectionPolicy
    -> m CTx
sendMoney SendActions{..} passphrase moneySource dstDistr policy = do
    ws <- askWalletSnapshot
    updates <- withTxpLocalData getUtxoModifier
    when walletTxCreationDisabled $
        throwM err405
        { errReasonPhrase = "Transaction creation is disabled by configuration!"
        }

    let srcWallet = getMoneySourceWallet moneySource
    rootSk <- getSKById srcWallet
    checkPassMatches passphrase rootSk `whenNothing`
        throwM (RequestError "Passphrase doesn't match")

    addrMetas' <- getMoneySourceAddresses ws moneySource
    addrMetas <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Given money source has no addresses!")

    let srcAddrs = map (view wamAddress) addrMetas

    logDebug "sendMoney: processed addrs"

    let metasAndAdrresses = M.fromList $ zip (toList srcAddrs) (toList addrMetas)
    allSecrets <- getSecretKeys

    let getSigner addr = runIdentity $ do
          let addrMeta =
                  fromMaybe (error "Corresponding adress meta not found")
                            (M.lookup addr metasAndAdrresses)
          case runExcept $ getSKByAddressPure allSecrets (ShouldCheckPassphrase False) passphrase addrMeta of
              Left err -> throw err
              Right sk -> withSafeSignerUnsafe sk (pure passphrase) pure

    relatedAccount <- getSomeMoneySourceAccount ws moneySource
    outputs <- coinDistrToOutputs dstDistr
    let pendingAddrs = getPendingAddresses ws policy
    (th, dstAddrs) <-
        rewrapTxError "Cannot send transaction" $ do
            (txAux, inpTxOuts') <-
                prepareMTx (getOwnUtxos ws updates) getSigner pendingAddrs policy srcAddrs outputs (relatedAccount, passphrase)

            ts <- Just <$> getCurrentTimestamp
            let tx = taTx txAux
                txHash = hash tx
                inpTxOuts = toList inpTxOuts'
                dstAddrs  = map txOutAddress . toList $
                            _txOutputs tx
                th = THEntry txHash tx Nothing inpTxOuts dstAddrs ts
            ptx <- mkPendingTx ws srcWallet txHash txAux th

            (th, dstAddrs) <$ submitAndSaveNewPtx enqueueMsg ptx

    logInfoS $
        sformat ("Successfully spent money from "%
                    listF ", " addressF % " addresses on " %
                    listF ", " addressF)
        (toList srcAddrs)
        dstAddrs

    -- XXX rewrite
    db <- askWalletDB
    addHistoryTx db srcWallet th
    diff <- getCurChainDifficulty
    ws' <- getWalletSnapshot db
    let srcWalletAddrsDetector = getWalletAddrsDetector ws' Ever srcWallet

    logDebug "sendMoney: constructing response"
    fst <$> constructCTx ws' srcWallet srcWalletAddrsDetector diff th
  where
     -- TODO eliminate copy-paste
     listF separator formatter =
         F.later $ fold . intersperse separator . fmap (F.bprint formatter)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

notFasterThan :: Second -> WalletWebMode a -> WalletWebMode a
notFasterThan time action = fst <$> concurrently action (delay time)
