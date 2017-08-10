-- | Notifier logic

module Pos.Wallet.Web.Sockets.Notifier
       ( launchNotifier
       ) where

import           Universum

import           Control.Concurrent               (forkFinally)
import           Control.Lens                     (each, has, ix, traversed, (.=))
import           Control.Monad.Catch              (SomeException, try)
import qualified Control.Monad.Catch              as E
import           Control.Monad.State              (runStateT)
import qualified Data.Aeson                       as A
import           Data.ByteString.Base58           (bitcoinAlphabet, decodeBase58)
import qualified Data.ByteString.Lazy             as BSL
import           Data.Default                     (Default (def))
import qualified Data.DList                       as DL
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (findIndex, notElem)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Set                         as S
import qualified Data.Text.Buildable
import           Data.Time.Clock.POSIX            (getPOSIXTime)
import           Data.Time.Units                  (Microsecond, Second)
import           Ether.Internal                   (HasLens (..))
import           Formatting                       (bprint, build, sformat, shown, stext,
                                                   (%))
import qualified Formatting                       as F
import           Network.Wai                      (Application)
import           Pos.ReportServer.Report          (ReportType (RInfo))
import           Serokell.AcidState.ExtendedState (ExtendedState)
import           Serokell.Util                    (threadDelay)
import qualified Serokell.Util.Base64             as B64
import           Serokell.Util.Text               (listJson)
import           Servant.API                      ((:<|>) ((:<|>)))
import           Servant.Multipart                (fdFilePath)
import           Servant.Server                   (Handler, Server, ServerT, runHandler,
                                                   serve)
import           Servant.Utils.Enter              ((:~>) (..), enter)
import           System.IO.Error                  (isDoesNotExistError)
import           System.Wlog                      (logDebug, logError, logInfo,
                                                   logWarning)

import           Pos.Aeson.ClientTypes            ()
import           Pos.Aeson.WalletBackup           ()
import           Pos.Binary.Class                 (biSize)
import           Pos.Block.Logic.Util             (withBlkSemaphore_)
import           Pos.Client.Txp.Balances          (getOwnUtxos)
import           Pos.Client.Txp.History           (TxHistoryEntry (..))
import           Pos.Client.Txp.Util              (TxError (..), createMTx,
                                                   overrideTxDistrBoot,
                                                   overrideTxOutDistrBoot)
import           Pos.Communication                (OutSpecs, SendActions (..), sendTxOuts,
                                                   submitMTx, submitRedemptionTx)
import           Pos.Constants                    (curSoftwareVersion, isDevelopment)
import           Pos.Context                      (GenesisUtxo)
import           Pos.Core                         (Address (..), Coin, TxFeePolicy (..),
                                                   TxSizeLinear (..), addressF,
                                                   bvdTxFeePolicy, calculateTxSizeLinear,
                                                   decodeTextAddress, getCurrentTimestamp,
                                                   getTimestamp, integerToCoin,
                                                   makeRedeemAddress, mkCoin, sumCoins,
                                                   unsafeAddCoin, unsafeIntegerToCoin,
                                                   unsafeSubCoin, _RedeemAddress)
import           Pos.Crypto                       (EncryptedSecretKey, PassPhrase,
                                                   SafeSigner, aesDecrypt,
                                                   changeEncPassphrase, checkPassMatches,
                                                   deriveAesKeyBS, emptyPassphrase,
                                                   fakeSigner, hash, keyGen,
                                                   redeemDeterministicKeyGen,
                                                   redeemToPublic, withSafeSigner,
                                                   withSafeSigner)
import           Pos.DB.Class                     (gsAdoptedBVData)
import           Pos.Genesis                      (genesisDevHdwSecretKeys)
import           Pos.Reporting.MemState           (HasReportServers (..),
                                                   HasReportingContext (..))
import           Pos.Reporting.Methods            (sendReport, sendReportNodeNologs)
import           Pos.Txp                          (TxFee (..))
import           Pos.Txp.Core                     (TxAux (..), TxOut (..), TxOutAux (..),
                                                   TxOutDistribution)
import           Pos.Util                         (eitherToThrow, maybeThrow)
import           Pos.Util.BackupPhrase            (toSeed)
import qualified Pos.Util.Modifier                as MM
import           Pos.Util.Servant                 (decodeCType, encodeCType)
import           Pos.Util.UserSecret              (UserSecretDecodingError (..),
                                                   readUserSecret, usWalletSet)
import           Pos.Wallet.KeyStorage            (addSecretKey, deleteSecretKey,
                                                   getSecretKeys)
import           Pos.Wallet.SscType               (WalletSscType)
import           Pos.Wallet.WalletMode            (applyLastUpdate,
                                                   blockchainSlotDuration, connectedPeers,
                                                   getBalance, getLocalHistory,
                                                   localChainDifficulty,
                                                   networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.Account           (AddrGenSeed, GenSeed (..),
                                                   MonadKeySearch (..), genSaveRootKey,
                                                   genUniqueAccountAddress,
                                                   genUniqueAccountId, getAddrIdx,
                                                   getSKById, myRootAddresses)
import           Pos.Wallet.Web.Api               (WalletApi, walletApi)
import           Pos.Wallet.Web.Backup            (AccountMetaBackup (..),
                                                   StateBackup (..), WalletBackup (..),
                                                   WalletMetaBackup (..), getStateBackup)
import           Pos.Wallet.Web.ClientTypes       (AccountId (..), Addr, CAccount (..),
                                                   CAccountId (..), CAccountInit (..),
                                                   CAccountMeta (..), CAddress (..),
                                                   CCoin, CElectronCrashReport (..), CId,
                                                   CInitialized,
                                                   CPaperVendWalletRedeem (..), CProfile,
                                                   CProfile (..), CTx (..), CTxId,
                                                   CTxMeta (..), CTxs (..),
                                                   CUpdateInfo (..), CWAddressMeta (..),
                                                   CWallet (..), CWalletInit (..),
                                                   CWalletMeta (..), CWalletRedeem (..),
                                                   NotifyEvent (..), SyncProgress (..),
                                                   Wal, addrMetaToAccount, cIdToAddress,
                                                   coinFromCCoin, encToCId, mkCCoin,
                                                   mkCTxs, spLocalCD, spNetworkCD,
                                                   spPeers, toCUpdateInfo, txIdToCTxId)
import           Pos.Wallet.Web.Error             (WalletError (..), rewrapToWalletError)
import qualified Pos.Wallet.Web.Mode
import           Pos.Wallet.Web.Secret            (WalletUserSecret (..),
                                                   mkGenesisWalletUserSecret, wusAccounts,
                                                   wusWalletName)
import           Pos.Wallet.Web.Sockets           (ConnectionsVar, closeWSConnections,
                                                   getWalletWebSockets, initWSConnections,
                                                   notifyAll, upgradeApplicationWS)
import           Pos.Wallet.Web.State             (AddressLookupMode (Ever, Existing), CustomAddressType (ChangeAddr, UsedAddr),
                                                   addOnlyNewTxMeta, addUpdate,
                                                   addWAddress, closeState, createAccount,
                                                   createWallet, getAccountIds,
                                                   getAccountMeta, getAccountWAddresses,
                                                   getHistoryCache, getNextUpdate,
                                                   getProfile, getTxMeta,
                                                   getWalletAddresses, getWalletMeta,
                                                   getWalletPassLU, isCustomAddress,
                                                   openState, removeAccount,
                                                   removeHistoryCache, removeNextUpdate,
                                                   removeTxMetas, removeWallet,
                                                   setAccountMeta, setProfile,
                                                   setWalletMeta, setWalletPassLU,
                                                   setWalletSyncTip, setWalletTxMeta,
                                                   testReset, updateHistoryCache)
import           Pos.Wallet.Web.State.Storage     (WalletStorage)
import           Pos.Wallet.Web.Tracking          (CAccModifier (..), CachedCAccModifier,
                                                   fixCachedAccModifierFor,
                                                   fixingCachedAccModifier,
                                                   sortedInsertions, syncWalletOnImport,
                                                   syncWalletsWithGState)
import           Pos.Wallet.Web.Util              (getWalletAccountIds, rewrapTxError)
import           Pos.Web                          (TlsParams, serveImpl)

-- FIXME: this is really inefficient. Temporary solution
launchNotifier :: WalletWebMode m => (m :~> Handler) -> m ()
launchNotifier nat =
    void . liftIO $ mapM startForking
        [ dificultyNotifier
        , updateNotifier
        ]
  where
    cooldownPeriod :: Second
    cooldownPeriod = 5

    difficultyNotifyPeriod :: Microsecond
    difficultyNotifyPeriod = 500000  -- 0.5 sec

    -- networkResendPeriod = 10         -- in delay periods
    forkForever action = forkFinally action $ const $ do
        -- TODO: log error
        -- cooldown
        threadDelay cooldownPeriod
        void $ forkForever action
    -- TODO: use Servant.enter here
    -- FIXME: don't ignore errors, send error msg to the socket
    startForking = forkForever . void . runHandler . ($$) nat
    notifier period action = forever $ do
        liftIO $ threadDelay period
        action
    dificultyNotifier = void . flip runStateT def $ notifier difficultyNotifyPeriod $ do
        whenJustM networkChainDifficulty $
            \networkDifficulty -> do
                oldNetworkDifficulty <- use spNetworkCD
                when (Just networkDifficulty /= oldNetworkDifficulty) $ do
                    lift $ notifyAll $ NetworkDifficultyChanged networkDifficulty
                    spNetworkCD .= Just networkDifficulty

        localDifficulty <- localChainDifficulty
        oldLocalDifficulty <- use spLocalCD
        when (localDifficulty /= oldLocalDifficulty) $ do
            lift $ notifyAll $ LocalDifficultyChanged localDifficulty
            spLocalCD .= localDifficulty

        peers <- connectedPeers
        oldPeers <- use spPeers
        when (peers /= oldPeers) $ do
            lift $ notifyAll $ ConnectedPeersChanged peers
            spPeers .= peers

    updateNotifier = do
        cps <- waitForUpdate
        addUpdate $ toCUpdateInfo cps
        logDebug "Added update to wallet storage"
        notifyAll UpdateAvailable

    -- historyNotifier :: WalletWebMode m => m ()
    -- historyNotifier = do
    --     cAddresses <- myCIds
    --     for_ cAddresses $ \cAddress -> do
    --         -- TODO: is reading from acid RAM only (not reading from disk?)
    --         oldHistoryLength <- length . fromMaybe mempty <$> getAccountHistory cAddress
    --         newHistoryLength <- length <$> getHistory cAddress
    --         when (oldHistoryLength /= newHistoryLength) .
    --             notifyAll $ NewWalletTransaction cAddress

