{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Methods.History
       (
       ) where

import           Universum

import           Control.Lens                 (each, has, ix, traversed)
import           Control.Monad.Catch          (SomeException, try)
import qualified Control.Monad.Catch          as E
import qualified Data.Aeson                   as A
import           Data.ByteString.Base58       (bitcoinAlphabet, decodeBase58)
import qualified Data.ByteString.Lazy         as BSL
import           Data.Default                 (Default (def))
import qualified Data.DList                   as DL
import qualified Data.HashMap.Strict          as HM
import qualified Data.List.NonEmpty           as NE
import qualified Data.Set                     as S
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Formatting                   (build, sformat, shown, stext, (%))
import qualified Formatting                   as F
import           Pos.ReportServer.Report      (ReportType (RInfo))
import qualified Serokell.Util.Base64         as B64
import           Servant.Multipart            (fdFilePath)
import           System.IO.Error              (isDoesNotExistError)
import           System.Wlog                  (logDebug, logError, logInfo, logWarning)

import           Pos.Aeson.ClientTypes        ()
import           Pos.Aeson.WalletBackup       ()
import           Pos.Binary.Class             (biSize)
import           Pos.Block.Logic.Util         (withBlkSemaphore_)
import           Pos.Client.Txp.Balances      (getOwnUtxos)
import           Pos.Client.Txp.History       (TxHistoryEntry (..))
import           Pos.Client.Txp.Util          (TxError (..), createMTx,
                                               overrideTxDistrBoot,
                                               overrideTxOutDistrBoot)
import           Pos.Communication            (SendActions (..), submitMTx,
                                               submitRedemptionTx)
import           Pos.Constants                (isDevelopment)
import           Pos.Core                     (Coin, TxFeePolicy (..), TxSizeLinear (..),
                                               addressF, bvdTxFeePolicy,
                                               calculateTxSizeLinear, decodeTextAddress,
                                               getCurrentTimestamp, getTimestamp,
                                               integerToCoin, makeRedeemAddress, mkCoin,
                                               unsafeAddCoin, unsafeSubCoin,
                                               _RedeemAddress)
import           Pos.Crypto                   (EncryptedSecretKey, PassPhrase, SafeSigner,
                                               aesDecrypt, deriveAesKeyBS,
                                               emptyPassphrase, fakeSigner, hash, keyGen,
                                               redeemDeterministicKeyGen, redeemToPublic,
                                               withSafeSigner, withSafeSigner)
import           Pos.DB.Class                 (gsAdoptedBVData)
import           Pos.Genesis                  (genesisDevHdwSecretKeys)
import           Pos.Reporting.MemState       (HasReportServers (..),
                                               HasReportingContext (..))
import           Pos.Reporting.Methods        (sendReport, sendReportNodeNologs)
import           Pos.Txp                      (TxFee (..))
import           Pos.Txp.Core                 (TxAux (..), TxOut (..), TxOutAux (..),
                                               TxOutDistribution)
import           Pos.Util                     (eitherToThrow, maybeThrow)
import           Pos.Util.BackupPhrase        (toSeed)
import           Pos.Util.UserSecret          (UserSecretDecodingError (..),
                                               readUserSecret, usWalletSet)
import           Pos.Wallet.KeyStorage        (addSecretKey, deleteSecretKey,
                                               getSecretKeys)
import           Pos.Wallet.WalletMode        (applyLastUpdate, connectedPeers,
                                               getLocalHistory, localChainDifficulty,
                                               networkChainDifficulty)
import           Pos.Wallet.Web.Account       (GenSeed (..), MonadKeySearch (..),
                                               genSaveRootKey, genUniqueAccountId)
import           Pos.Wallet.Web.Backup        (AccountMetaBackup (..), StateBackup (..),
                                               WalletBackup (..), WalletMetaBackup (..),
                                               getStateBackup)
import           Pos.Wallet.Web.ClientTypes   (AccountId (..), Addr, CAccountId (..),
                                               CAccountInit (..), CAccountMeta (..),
                                               CAddress (..), CCoin,
                                               CElectronCrashReport (..), CId,
                                               CInitialized, CPaperVendWalletRedeem (..),
                                               CProfile, CProfile (..), CTx (..), CTxId,
                                               CTxMeta (..), CTxs (..), CUpdateInfo (..),
                                               CWAddressMeta (..), CWallet (..),
                                               CWalletInit (..), CWalletMeta (..),
                                               CWalletRedeem (..), SyncProgress (..), Wal,
                                               addrMetaToAccount, encToCId, mkCCoin,
                                               mkCTxs, txIdToCTxId)
import           Pos.Wallet.Web.Error         (WalletError (..), rewrapToWalletError)
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Secret        (WalletUserSecret (..),
                                               mkGenesisWalletUserSecret, wusAccounts,
                                               wusWalletName)
import           Pos.Wallet.Web.State         (AddressLookupMode (Ever, Existing),
                                               addOnlyNewTxMeta, createAccount,
                                               getHistoryCache, getNextUpdate, getProfile,
                                               getTxMeta, getWalletMeta, removeNextUpdate,
                                               setProfile, setWalletSyncTip,
                                               setWalletTxMeta, testReset,
                                               updateHistoryCache)
import           Pos.Wallet.Web.Tracking      (fixingCachedAccModifier,
                                               syncWalletOnImport)
import           Pos.Wallet.Web.Util          (getWalletAccountIds, rewrapTxError)


getFullWalletHistory :: MonadWalletWebMode m => CId Wal -> m ([CTx], Word)
getFullWalletHistory cWalId = do
    addrs <- mapM L.decodeCIdOrFail =<< L.getWalletAddrs Ever cWalId

    blockHistory <- getHistoryCache cWalId >>= \case
        Just hist -> pure $ DL.fromList hist
        Nothing -> do
            logWarning $
                sformat ("getFullWalletHistory: history cache is empty for wallet #"%build)
                cWalId
            pure mempty

    localHistory <- getLocalHistory addrs

    let fullHistory = DL.toList $ localHistory <> blockHistory
    ctxs <- forM fullHistory $ addHistoryTx cWalId
    let cHistory = concatMap toList ctxs
    pure (cHistory, fromIntegral $ length cHistory)

getHistory
    :: MonadWalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> m ([CTx], Word)
getHistory mCWalId mAccountId mAddrId = do
    -- FIXME: searching when only AddrId is provided is not supported yet.
    (cWalId, accIds) <- case (mCWalId, mAccountId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) -> do
            accIds' <- getWalletAccountIds cWalId'
            pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, [accId])
    accAddrs <- map cwamId <$> concatMapM (L.getAccountAddrsOrThrow Ever) accIds
    addrs <- case mAddrId of
        Nothing -> pure accAddrs
        Just addr ->
            if addr `elem` accAddrs then pure [addr] else throwM errorBadAddress
    first (filter (fits addrs)) <$> getFullWalletHistory cWalId
  where
    fits :: [CId Addr] -> CTx -> Bool
    fits addrs ctx = any (relatesToAddr ctx) addrs
    relatesToAddr CTx {..} = (`elem` (ctInputAddrs ++ ctOutputAddrs))
    errorSpecifySomething = RequestError $
        "Please specify either walletId or accountId"
    errorDontSpecifyBoth = RequestError $
        "Please do not specify both walletId and accountId at the same time"
    errorBadAddress = RequestError $
        "Specified wallet/account does not contain specified address"

getHistoryLimited
    :: MonadWalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> Maybe Word
    -> Maybe Word
    -> m ([CTx], Word)
getHistoryLimited mCWalId mAccId mAddrId mSkip mLimit =
    first applySkipLimit <$> getHistory mCWalId mAccId mAddrId
  where
    applySkipLimit = take limit . drop skip
    limit = (fromIntegral $ fromMaybe defaultLimit mLimit)
    skip = (fromIntegral $ fromMaybe defaultSkip mSkip)
    defaultLimit = 100
    defaultSkip = 0

addHistoryTx
    :: MonadWalletWebMode m
    => CId Wal
    -> TxHistoryEntry
    -> m CTxs
addHistoryTx cWalId wtx@THEntry{..} = do
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    meta <- CTxMeta <$> case _thTimestamp of
      Nothing -> liftIO $ getPOSIXTime
      Just ts -> return $ fromIntegral (getTimestamp ts) / 1000000
    let cId = txIdToCTxId _thTxId
    addOnlyNewTxMeta cWalId cId meta
    meta' <- fromMaybe meta <$> getTxMeta cWalId cId
    walAddrMetas <- L.getWalletAddrMetas Ever cWalId
    mkCTxs diff wtx meta' walAddrMetas & either (throwM . InternalError) pure

