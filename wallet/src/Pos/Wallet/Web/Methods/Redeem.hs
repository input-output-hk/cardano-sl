{-# LANGUAGE TypeFamilies #-}

-- | Wallet redemption

module Pos.Wallet.Web.Methods.Redeem
       ( redeemAda
       , redeemAdaPaperVend
       ) where

import           Universum

import           Data.ByteString.Base58         (bitcoinAlphabet, decodeBase58)
import qualified Serokell.Util.Base64           as B64

import           Pos.Client.Txp.History         (TxHistoryEntry (..))
import           Pos.Communication              (prepareRedemptionTx)
import           Pos.Core                       (getCurrentTimestamp)
import           Pos.Crypto                     (PassPhrase, aesDecrypt, deriveAesKeyBS,
                                                 hash, redeemDeterministicKeyGen)
import           Pos.Txp.Core                   (TxAux (..), TxOut (..))
import           Pos.Util                       (maybeThrow)
import           Pos.Util.BackupPhrase          (toSeed)
import           Pos.Wallet.Web.Account         (GenSeed (..))
import           Pos.Wallet.Web.ClientTypes     (AccountId (..), CAccountId (..),
                                                 CAddress (..),
                                                 CPaperVendWalletRedeem (..), CTx (..),
                                                 CWalletRedeem (..))
import           Pos.Wallet.Web.Error           (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTxMeta, constructCTx,
                                                 getCurChainDifficulty)
import qualified Pos.Wallet.Web.Methods.Logic   as L
import           Pos.Wallet.Web.Methods.Txp     (MonadWalletTxFull, rewrapTxError,
                                                 submitAndSaveNewPtx)
import           Pos.Wallet.Web.Pending         (mkPendingTx)
import           Pos.Wallet.Web.State           (AddressLookupMode (Ever))
import           Pos.Wallet.Web.Tracking        (fixingCachedAccModifier)
import           Pos.Wallet.Web.Util            (decodeCTypeOrFail, getWalletAddrsSet)


redeemAda
    :: MonadWalletTxFull ctx m
    => PassPhrase -> CWalletRedeem -> m CTx
redeemAda passphrase CWalletRedeem {..} = do
    seedBs <- maybe invalidBase64 pure
        -- NOTE: this is just safety measure
        $ rightToMaybe (B64.decode crSeed) <|> rightToMaybe (B64.decodeUrl crSeed)
    redeemAdaInternal passphrase crWalletId seedBs
  where
    invalidBase64 =
        throwM . RequestError $ "Seed is invalid base64(url) string: " <> crSeed

-- Decrypts certificate based on:
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L205
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L160
redeemAdaPaperVend
    :: MonadWalletTxFull ctx m
    => PassPhrase
    -> CPaperVendWalletRedeem
    -> m CTx
redeemAdaPaperVend passphrase CPaperVendWalletRedeem {..} = do
    seedEncBs <- maybe invalidBase58 pure
        $ decodeBase58 bitcoinAlphabet $ encodeUtf8 pvSeed
    aesKey <- either invalidMnemonic pure
        $ deriveAesKeyBS <$> toSeed pvBackupPhrase
    seedDecBs <- either decryptionFailed pure
        $ aesDecrypt seedEncBs aesKey
    redeemAdaInternal passphrase pvWalletId seedDecBs
  where
    invalidBase58 =
        throwM . RequestError $ "Seed is invalid base58 string: " <> pvSeed
    invalidMnemonic e =
        throwM . RequestError $ "Invalid mnemonic: " <> toText e
    decryptionFailed e =
        throwM . RequestError $ "Decryption failed: " <> show e

redeemAdaInternal
    :: MonadWalletTxFull ctx m
    => PassPhrase
    -> CAccountId
    -> ByteString
    -> m CTx
redeemAdaInternal passphrase cAccId seedBs = do
    (_, redeemSK) <- maybeThrow (RequestError "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    accId <- decodeCTypeOrFail cAccId
    -- new redemption wallet
    _ <- fixingCachedAccModifier L.getAccount accId

    dstAddr <- decodeCTypeOrFail . cadId =<<
               L.newAddress RandomSeed passphrase accId
    th <- rewrapTxError "Cannot send redemption transaction" $ do
        (txAux, redeemAddress, redeemBalance) <-
                prepareRedemptionTx redeemSK dstAddr

        ts <- Just <$> getCurrentTimestamp
        let tx = taTx txAux
            txHash = hash tx
            txInputs = [TxOut redeemAddress redeemBalance]
            th = THEntry txHash tx Nothing txInputs [dstAddr] ts
            dstWallet = aiWId accId
        ptx <- mkPendingTx dstWallet txHash txAux th

        th <$ submitAndSaveNewPtx ptx

    -- add redemption transaction to the history of new wallet
    let cWalId = aiWId accId
    -- We add TxHistoryEntry's meta created by us in advance
    -- to make TxHistoryEntry in CTx consistent with entry in history.
    addHistoryTxMeta cWalId th
    cWalAddrs <- getWalletAddrsSet Ever cWalId
    diff <- getCurChainDifficulty
    fst <$> constructCTx cWalId cWalAddrs diff th
