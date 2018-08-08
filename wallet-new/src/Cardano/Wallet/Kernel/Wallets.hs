module Cardano.Wallet.Kernel.Wallets (
      createHdWallet
    , updateHdWallet
    , updatePassword
    , deleteHdWallet
      -- * Errors
    , CreateWalletError(..)
    , UpdateWalletPasswordError(..)
    -- * Internal & testing use only
    , createWalletHdRnd
    ) where

import qualified Prelude
import           Universum

import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F
import qualified Formatting.Buildable

import           Data.Acid.Advanced (update')

import           Pos.Chain.Txp (Utxo)
import           Pos.Core (Timestamp)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase,
                     changeEncPassphrase, checkPassMatches, emptyPassphrase,
                     safeDeterministicKeyGen)

import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.BIP39 (Mnemonic)
import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import           Cardano.Wallet.Kernel.DB.AcidState (CreateHdWallet (..),
                     DeleteHdRoot (..), UpdateHdRootPassword (..),
                     UpdateHdWallet (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel, HdRoot,
                     WalletName, eskToHdRootId)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.Internal (PassiveWallet, walletKeystore,
                     wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.PrefilterTx (prefilterUtxo)
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util (getCurrentTimestamp)

import           Test.QuickCheck (Arbitrary (..), oneof)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data CreateWalletError =
      CreateWalletFailed HD.CreateHdRootError
      -- ^ When trying to create the 'Wallet', the DB operation failed.

instance Arbitrary CreateWalletError where
    arbitrary = oneof []

instance Buildable CreateWalletError where
    build (CreateWalletFailed dbOperation) =
        bprint ("CreateWalletUnknownHdAccount " % F.build) dbOperation

instance Show CreateWalletError where
    show = formatToString build

instance Exception CreateWalletError

data UpdateWalletPasswordError =
      UpdateWalletPasswordOldPasswordMismatch HD.HdRootId
      -- ^ When trying to update the wallet password, there was a mismatch
      -- with the old one.
    | UpdateWalletPasswordKeyNotFound HD.HdRootId
      -- ^ When trying to update the wallet password, there was no
      -- 'EncryptedSecretKey' in the Keystore for this 'HdRootId'.
    | UpdateWalletPasswordChangeFailed HD.HdRootId
      -- ^ When trying to shield the 'SecretKey' with the new, supplied
      -- 'PassPhrase', the crypto primitive responsible for that failed.
    | UpdateWalletPasswordUnknownHdRoot HD.UnknownHdRoot
      -- ^ When trying to update the DB the input 'HdRootId' was not found.

instance Arbitrary UpdateWalletPasswordError where
    arbitrary = oneof []

instance Buildable UpdateWalletPasswordError where
    build (UpdateWalletPasswordOldPasswordMismatch hdRootId) =
        bprint ("UpdateWalletPasswordOldPasswordMismatch " % F.build) hdRootId
    build (UpdateWalletPasswordKeyNotFound hdRootId) =
        bprint ("UpdateWalletPasswordKeyNotFound " % F.build) hdRootId
    build (UpdateWalletPasswordChangeFailed hdRootId) =
        bprint ("UpdateWalletPasswordChangeFailed " % F.build) hdRootId
    build (UpdateWalletPasswordUnknownHdRoot uRoot) =
        bprint ("UpdateWalletPasswordUnknownHdRoot " % F.build) uRoot

instance Show UpdateWalletPasswordError where
    show = formatToString build

instance Exception UpdateWalletPasswordError

{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

-- | Creates a new HD 'Wallet'.
-- INVARIANT: The input 'Mnemonic' should be supplied by the frontend such that
-- this is a brand new 'Mnemonic' never used before on the blockchain. Failing
-- to do so would cause an invariant violation as we system would treat this
-- wallet as a new one rather than dealing with a proper restoration.
--
createHdWallet :: PassiveWallet
             -> Mnemonic nat
             -- ^ The set of words (i.e the mnemonic) to generate the initial seed.
             -- See <https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#From_mnemonic_to_seed>
             -- This Kernel function is agnostic in the number of words, and it's
             -- wallet layer's responsibility to make sure that invalid sizes are
             -- rejected.
             -> PassPhrase
             -- ^ The spending password to encrypt the 'SecretKey' for the
             -- newly-generated wallet. If the user didn't specify any, the
             -- empty 'PassPhrase' is used.
             -> AssuranceLevel
             -- ^ The 'AssuranceLevel' for this wallet, namely after how many
             -- blocks each transaction is considered 'adopted'. This translates
             -- in the frontend with a different threshold for the confirmation
             -- range (@low@, @medium@, @high@).
             -> WalletName
             -- ^ The name for this wallet.
             -> IO (Either CreateWalletError HdRoot)
createHdWallet pw mnemonic spendingPassword assuranceLevel walletName = do
    -- STEP 1: Generate the 'EncryptedSecretKey' outside any acid-state
    -- transaction, to not leak it into acid-state's transaction logs.
    let (_, esk) = safeDeterministicKeyGen (BIP39.mnemonicToSeed mnemonic) spendingPassword
    -- STEP 2: Atomically generate the wallet and the initial internal structure in
    -- an acid-state transaction.
    let newRootId = eskToHdRootId esk
    res <- createWalletHdRnd pw
                             (spendingPassword /= emptyPassphrase)
                             walletName
                             assuranceLevel
                             esk
                             mempty -- ^ Brand new wallets have no Utxo.
                                    --   See the invariant at the top.
    case res of
         Left e   -> return . Left $ CreateWalletFailed e
         Right hdRoot -> do
             -- STEP 3: Insert the 'EncryptedSecretKey' into the 'Keystore'
             Keystore.insert (WalletIdHdRnd newRootId) esk (pw ^. walletKeystore)
             return (Right hdRoot)


-- | Creates an HD wallet where new accounts and addresses are generated
-- via random index derivation.
--
-- Prefilters the Utxo before passing it to the Acidstate update.
--
-- Adds an HdRoot and HdAccounts (which are discovered during prefiltering of utxo).
-- In the case of empty utxo, no HdAccounts are created.
-- Fails with CreateHdWalletError if the HdRootId already exists.
createWalletHdRnd :: PassiveWallet
                  -> Bool
                  -- ^ Whether or not this wallet has a spending password set.
                  -> HD.WalletName
                  -> AssuranceLevel
                  -> EncryptedSecretKey
                  -> Utxo
                  -> IO (Either HD.CreateHdRootError HdRoot)
createWalletHdRnd pw hasSpendingPassword name assuranceLevel esk utxo = do
    created <- InDb <$> getCurrentTimestamp
    let rootId  = eskToHdRootId esk
        newRoot = HD.initHdRoot rootId
                                name
                                (hdSpendingPassword created)
                                assuranceLevel
                                created
        utxoByAccount = prefilterUtxo rootId esk utxo

    res <- update' (pw ^. wallets) $ CreateHdWallet newRoot utxoByAccount
    return $ case res of
                 Left err -> Left err
                 Right () -> Right newRoot
    where

        hdSpendingPassword :: InDb Timestamp -> HD.HasSpendingPassword
        hdSpendingPassword created =
            if hasSpendingPassword then HD.HasSpendingPassword created
                                   else HD.NoSpendingPassword

deleteHdWallet :: PassiveWallet
               -> HD.HdRootId
               -> IO (Either HD.UnknownHdRoot ())
deleteHdWallet wallet rootId = do
    -- STEP 1: Remove the HdRoot via an acid-state transaction which will
    --         also delete any associated accounts and addresses.
    res <- update' (wallet ^. wallets) $ DeleteHdRoot rootId
    case res of
        Left err -> return (Left err)
        Right () -> do
            -- STEP 2: Purge the key from the keystore.
            Keystore.delete (WalletIdHdRnd rootId) (wallet ^. walletKeystore)
            return $ Right ()

{-------------------------------------------------------------------------------
  Wallet update
-------------------------------------------------------------------------------}

updateHdWallet :: PassiveWallet
               -> HD.HdRootId
               -> HD.AssuranceLevel
               -> HD.WalletName
               -> IO (Either HD.UnknownHdRoot (Kernel.DB, HdRoot))
updateHdWallet pw hdRootId assuranceLevel walletName = do
    res <- update' (pw ^. wallets) (UpdateHdWallet hdRootId assuranceLevel walletName)
    case res of
         Left e              -> return (Left e)
         Right (db, newRoot) -> return (Right (db, newRoot))

updatePassword :: PassiveWallet
               -> HD.HdRootId
               -> PassPhrase
               -- ^ The old 'PassPhrase' for this Wallet.
               -> PassPhrase
               -- ^ The new 'PassPhrase' for this Wallet.
               -> IO (Either UpdateWalletPasswordError (Kernel.DB, HdRoot))
updatePassword pw hdRootId oldPassword newPassword = do
    let keystore = pw ^. walletKeystore
        wId = WalletIdHdRnd hdRootId
    -- STEP 1: Lookup the key from the keystore
    mbKey <- Keystore.lookup wId keystore
    case mbKey of
         Nothing -> return $ Left $ UpdateWalletPasswordKeyNotFound hdRootId
         Just esk -> do
             -- STEP 2: Check that the 2 password matches. While this in
             --         principle could be checked on the wallet layer side,
             --         it's preferrable to do everything in the kernel to make
             --         this operation really atomic.
             let pwdCheck = hoistMaybeWith (UpdateWalletPasswordOldPasswordMismatch hdRootId) $
                            checkPassMatches oldPassword esk

             -- STEP 3: Compute the new key using the cryptographic primitives
             --         we have. This operation doesn't change any state, it
             --         just recomputes the new 'EncryptedSecretKey' in-place.
             genNewKey <- changeEncPassphrase oldPassword newPassword esk
             let mbNewKey = hoistMaybeWith (UpdateWalletPasswordChangeFailed hdRootId) $
                            genNewKey

             case pwdCheck >> mbNewKey of
                  Left e -> return (Left e)
                  Right newKey -> do
                      -- STEP 4: Update the keystore, atomically.
                      Keystore.replace wId newKey keystore
                      -- STEP 5: Update the timestamp in the wallet data storage.
                      lastUpdateNow <- InDb <$> getCurrentTimestamp
                      let hasSpendingPassword = HD.HasSpendingPassword lastUpdateNow
                      res <- update' (pw ^. wallets)
                                     (UpdateHdRootPassword hdRootId hasSpendingPassword)
                      case res of
                           Left e ->
                               return $ Left (UpdateWalletPasswordUnknownHdRoot e)
                           Right (db, hdRoot') -> return $ Right (db, hdRoot')

-- | Hoist a Maybe into an Either so that multiple calls that may fail can
-- be chained.
hoistMaybeWith :: e -> Maybe a -> Either e a
hoistMaybeWith err Nothing = Left err
hoistMaybeWith _ (Just r)  = Right r
