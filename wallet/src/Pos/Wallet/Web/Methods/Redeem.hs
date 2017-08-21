{-# LANGUAGE TypeFamilies #-}

-- | Wallet redemption

module Pos.Wallet.Web.Methods.Redeem
       ( redeemAda
       , redeemAdaPaperVend
       ) where

import           Universum

import           Data.ByteString.Base58         (bitcoinAlphabet, decodeBase58)
import qualified Serokell.Util.Base64           as B64

import           Pos.Aeson.ClientTypes          ()
import           Pos.Aeson.WalletBackup         ()
import           Pos.Client.Txp.Addresses       (MonadAddresses)
import           Pos.Client.Txp.History         (TxHistoryEntry (..))
import           Pos.Communication              (SendActions (..), submitRedemptionTx)
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
                                                 CTxs (..), CWalletRedeem (..))
import           Pos.Wallet.Web.Error           (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTx)
import qualified Pos.Wallet.Web.Methods.Logic   as L
import           Pos.Wallet.Web.Mode            (MonadWalletWebMode)
import           Pos.Wallet.Web.Tracking        (fixingCachedAccModifier)
import           Pos.Wallet.Web.Util            (decodeCTypeOrFail, rewrapTxError)


redeemAda
    :: (MonadWalletWebMode m, MonadAddresses m)
    => SendActions m -> PassPhrase -> CWalletRedeem -> m CTx
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
    :: (MonadWalletWebMode m, MonadAddresses m)
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
    :: (MonadWalletWebMode m, MonadAddresses m)
    => SendActions m
    -> PassPhrase
    -> CAccountId
    -> ByteString
    -> m CTx
redeemAdaInternal SendActions {..} passphrase cAccId seedBs = do
    (_, redeemSK) <- maybeThrow (RequestError "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    accId <- decodeCTypeOrFail cAccId
    -- new redemption wallet
    _ <- fixingCachedAccModifier L.getAccount accId

    dstAddr <- decodeCTypeOrFail . cadId =<<
               L.newAddress RandomSeed passphrase accId
    (TxAux {..}, redeemAddress, redeemBalance) <-
        rewrapTxError "Cannot send redemption transaction" $
        submitRedemptionTx enqueueMsg redeemSK dstAddr
    -- add redemption transaction to the history of new wallet
    let txInputs = [TxOut redeemAddress redeemBalance]
    ts <- Just <$> getCurrentTimestamp
    ctxs <- addHistoryTx (aiWId accId) $
        THEntry (hash taTx) taTx Nothing txInputs [dstAddr] ts
    ctsIncoming ctxs `whenNothing` throwM noIncomingTx
  where
    noIncomingTx = InternalError "Can't report incoming transaction"
