{-# LANGUAGE TypeFamilies #-}

-- | Crashes reporting

module Pos.Wallet.Web.Methods.Reporting
       ( reportingInitialized
       , reportingElectroncrash
       ) where

import           Universum

import           Control.Lens                   (has)
import           Control.Monad.Catch            (SomeException, try)
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set                       as S
import           Formatting                     (build, sformat, shown, (%))
import qualified Formatting                     as F
import           Pos.ReportServer.Report        (ReportType (RInfo))
import           Servant.Multipart              (fdFilePath)
import           System.Wlog                    (logDebug, logError, logInfo)

import           Pos.Aeson.ClientTypes          ()
import           Pos.Aeson.WalletBackup         ()
import           Pos.Binary.Class               (biSize)
import           Pos.Client.Txp.Balances        (getOwnUtxos)
import           Pos.Client.Txp.History         (TxHistoryEntry (..))
import           Pos.Client.Txp.Util            (TxError (..), createMTx,
                                                 overrideTxDistrBoot,
                                                 overrideTxOutDistrBoot)
import           Pos.Communication              (SendActions (..), submitMTx)
import           Pos.Core                       (Coin, TxFeePolicy (..),
                                                 TxSizeLinear (..), addressF,
                                                 bvdTxFeePolicy, calculateTxSizeLinear,
                                                 decodeTextAddress, getCurrentTimestamp,
                                                 integerToCoin, mkCoin, unsafeAddCoin,
                                                 unsafeSubCoin, _RedeemAddress)
import           Pos.Crypto                     (PassPhrase, fakeSigner, hash, keyGen,
                                                 withSafeSigners)
import           Pos.DB.Class                   (gsAdoptedBVData)
import           Pos.Reporting.MemState         (HasReportServers (..),
                                                 HasReportingContext (..))
import           Pos.Reporting.Methods          (sendReport, sendReportNodeNologs)
import           Pos.Txp                        (TxFee (..))
import           Pos.Txp.Core                   (TxAux (..), TxOut (..), TxOutAux (..),
                                                 TxOutDistribution)
import           Pos.Util                       (eitherToThrow, maybeThrow)
import           Pos.Wallet.KeyStorage          (deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode          (applyLastUpdate, connectedPeers,
                                                 localChainDifficulty,
                                                 networkChainDifficulty)
import           Pos.Wallet.Web.Account         (GenSeed (..), MonadKeySearch (..))
import           Pos.Wallet.Web.ClientTypes     (AccountId (..), Addr, CAddress (..),
                                                 CCoin, CElectronCrashReport (..), CId,
                                                 CInitialized, CProfile, CProfile (..),
                                                 CTx (..), CTxs (..), CUpdateInfo (..),
                                                 CWAddressMeta (..), SyncProgress (..),
                                                 Wal, addrMetaToAccount, mkCCoin)
import           Pos.Wallet.Web.Error           (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTx)
import qualified Pos.Wallet.Web.Methods.Logic   as L
import           Pos.Wallet.Web.Mode            (MonadWalletWebMode)
import           Pos.Wallet.Web.State           (AddressLookupMode (Existing),
                                                 getNextUpdate, getProfile,
                                                 removeNextUpdate, setProfile, testReset)
import           Pos.Wallet.Web.Util            (getWalletAccountIds, rewrapTxError)

reportingInitialized :: MonadWalletWebMode m => CInitialized -> m ()
reportingInitialized cinit = do
    sendReportNodeNologs (RInfo $ show cinit) `catchAll` handler
  where
    handler e =
        logError $
        sformat ("Didn't manage to report initialization time "%shown%
                 " because of exception "%shown) cinit e

reportingElectroncrash :: forall m. MonadWalletWebMode m => CElectronCrashReport -> m ()
reportingElectroncrash celcrash = do
    servers <- view (reportingContext . reportServers)
    errors <- fmap lefts $ forM servers $ \serv ->
        try $ sendReport [fdFilePath $ cecUploadDump celcrash]
                         []
                         (RInfo $ show celcrash)
                         "daedalus"
                         (toString serv)
    whenNotNull errors $ handler . NE.head
  where
    fmt = ("Didn't manage to report electron crash "%shown%" because of exception "%shown)
    handler :: SomeException -> m ()
    handler e = logError $ sformat fmt celcrash e

