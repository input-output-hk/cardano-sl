-- | Wallet redemption

module Pos.Wallet.Web.Methods.Redeem where

import           Universum

import           Control.Lens                   (has)
import           Control.Monad.Catch            (SomeException, try)
import           Data.ByteString.Base58         (bitcoinAlphabet, decodeBase58)
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set                       as S
import           Formatting                     (build, sformat, shown, (%))
import qualified Formatting                     as F
import           Pos.ReportServer.Report        (ReportType (RInfo))
import qualified Serokell.Util.Base64           as B64
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
import           Pos.Communication              (SendActions (..), submitMTx,
                                                 submitRedemptionTx)
import           Pos.Core                       (Coin, TxFeePolicy (..),
                                                 TxSizeLinear (..), addressF,
                                                 bvdTxFeePolicy, calculateTxSizeLinear,
                                                 decodeTextAddress, getCurrentTimestamp,
                                                 integerToCoin, makeRedeemAddress, mkCoin,
                                                 unsafeAddCoin, unsafeSubCoin,
                                                 _RedeemAddress)
import           Pos.Crypto                     (PassPhrase, aesDecrypt, deriveAesKeyBS,
                                                 fakeSigner, hash, keyGen,
                                                 redeemDeterministicKeyGen,
                                                 redeemToPublic, withSafeSigners)
import           Pos.DB.Class                   (gsAdoptedBVData)
import           Pos.Reporting.MemState         (HasReportServers (..),
                                                 HasReportingContext (..))
import           Pos.Reporting.Methods          (sendReport, sendReportNodeNologs)
import           Pos.Txp                        (TxFee (..))
import           Pos.Txp.Core                   (TxAux (..), TxOut (..), TxOutAux (..),
                                                 TxOutDistribution)
import           Pos.Util                       (eitherToThrow, maybeThrow)
import           Pos.Util.BackupPhrase          (toSeed)
import           Pos.Wallet.KeyStorage          (deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode          (applyLastUpdate, connectedPeers,
                                                 localChainDifficulty,
                                                 networkChainDifficulty)
import           Pos.Wallet.Web.Account         (GenSeed (..), MonadKeySearch (..))
import           Pos.Wallet.Web.ClientTypes     (AccountId (..), Addr, CAccountId (..),
                                                 CAddress (..), CCoin,
                                                 CElectronCrashReport (..), CId,
                                                 CInitialized,
                                                 CPaperVendWalletRedeem (..), CProfile,
                                                 CProfile (..), CTx (..), CTxs (..),
                                                 CUpdateInfo (..), CWAddressMeta (..),
                                                 CWalletRedeem (..), SyncProgress (..),
                                                 Wal, addrMetaToAccount, mkCCoin)
import           Pos.Wallet.Web.Error           (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTx)
import qualified Pos.Wallet.Web.Methods.Logic   as L
import           Pos.Wallet.Web.Mode            (MonadWalletWebMode)
import           Pos.Wallet.Web.State           (AddressLookupMode (Existing),
                                                 getNextUpdate, getProfile,
                                                 removeNextUpdate, setProfile, testReset)
import           Pos.Wallet.Web.Tracking        (fixingCachedAccModifier)
import           Pos.Wallet.Web.Util            (getWalletAccountIds, rewrapTxError)


redeemAda :: MonadWalletWebMode m => SendActions m -> PassPhrase -> CWalletRedeem -> m CTx
redeemAda sendActions passphrase CWalletRedeem {..} = do
    seedBs <- maybe invalidBase64 pure
        -- NOTE: this is just safety measure
        $ rightToMaybe (B64.decode crSeed) <|> rightToMaybe (B64.decodeUrl crSeed)
    redeemAdaInternal sendActions passphrase crWalletId seedBs
  where
    invalidBase64 =
        throwM . RequestError $ "Seed is invalid base64(url) string: " <> crSeed

-- Decrypts certificate based on:
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L205
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L160
redeemAdaPaperVend
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> CPaperVendWalletRedeem
    -> m CTx
redeemAdaPaperVend sendActions passphrase CPaperVendWalletRedeem {..} = do
    seedEncBs <- maybe invalidBase58 pure
        $ decodeBase58 bitcoinAlphabet $ encodeUtf8 pvSeed
    aesKey <- either invalidMnemonic pure
        $ deriveAesKeyBS <$> toSeed pvBackupPhrase
    seedDecBs <- either decryptionFailed pure
        $ aesDecrypt seedEncBs aesKey
    redeemAdaInternal sendActions passphrase pvWalletId seedDecBs
  where
    invalidBase58 =
        throwM . RequestError $ "Seed is invalid base58 string: " <> pvSeed
    invalidMnemonic e =
        throwM . RequestError $ "Invalid mnemonic: " <> toText e
    decryptionFailed e =
        throwM . RequestError $ "Decryption failed: " <> show e

redeemAdaInternal
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> CAccountId
    -> ByteString
    -> m CTx
redeemAdaInternal SendActions {..} passphrase cAccId seedBs = do
    (_, redeemSK) <- maybeThrow (RequestError "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    accId <- L.decodeCAccountIdOrFail cAccId
    -- new redemption wallet
    _ <- fixingCachedAccModifier L.getAccount accId

    let srcAddr = makeRedeemAddress $ redeemToPublic redeemSK
    dstAddr <- L.decodeCIdOrFail . cadId =<<
               L.newAddress RandomSeed passphrase accId
    (TxAux {..}, redeemAddress, redeemBalance) <-
        rewrapTxError "Cannot send redemption transaction" $
        submitRedemptionTx enqueueMsg redeemSK dstAddr
    -- add redemption transaction to the history of new wallet
    let txInputs = [TxOut redeemAddress redeemBalance]
    ts <- Just <$> getCurrentTimestamp
    ctxs <- addHistoryTx (aiWId accId) $
        THEntry (hash taTx) taTx txInputs Nothing [srcAddr] [dstAddr] ts
    ctsIncoming ctxs `whenNothing` throwM noIncomingTx
  where
    noIncomingTx = InternalError "Can't report incoming transaction"
