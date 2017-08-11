{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Methods.Backup
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

restoreWalletFromBackup :: MonadWalletWebMode m => WalletBackup -> m (Maybe CWallet)
restoreWalletFromBackup WalletBackup {..} = do
    let wId = encToCId wbSecretKey
    wExists <- isJust <$> getWalletMeta wId

    if wExists
        then do
            logWarning $
                sformat ("Wallet with id "%build%" already exists") wId
            pure Nothing
        else do
            let (WalletMetaBackup wMeta) = wbMeta
                accList = HM.toList wbAccounts
                          & each . _2 %~ \(AccountMetaBackup am) -> am

            addSecretKey wbSecretKey
            for_ accList $ \(idx, meta) -> do
                let aIdx = fromInteger $ fromIntegral idx
                    seedGen = DeterminedSeed aIdx
                accId <- genUniqueAccountId seedGen wId
                createAccount accId meta
            void $ L.createWalletSafe wId wMeta
            void $ syncWalletOnImport wbSecretKey
            -- Get wallet again to return correct balance and stuff
            Just <$> L.getWallet wId

restoreStateFromBackup :: MonadWalletWebMode m => StateBackup -> m [CWallet]
restoreStateFromBackup (FullStateBackup walletBackups) =
    catMaybes <$> forM walletBackups restoreWalletFromBackup

importStateJSON :: MonadWalletWebMode m => Text -> m [CWallet]
importStateJSON (toString -> fp) = do
    contents <- liftIO $ BSL.readFile fp
    wState <- either parseErr pure $ A.eitherDecode contents
    restoreStateFromBackup wState
  where
    parseErr err = throwM . RequestError $
        sformat ("Error while reading JSON backup file: "%stext) $
        toText err

exportStateJSON :: MonadWalletWebMode m => Text -> m ()
exportStateJSON (toString -> fp) = do
    wState <- getStateBackup
    liftIO $ BSL.writeFile fp $ A.encode wState

