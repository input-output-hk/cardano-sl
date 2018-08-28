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

import           Pos.Core (Timestamp)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase,
                     changeEncPassphrase, checkPassMatches, emptyPassphrase,
                     safeDeterministicKeyGen)

import           Cardano.Wallet.Kernel.BIP39 (Mnemonic)
import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import           Cardano.Wallet.Kernel.DB.AcidState (CreateHdWallet (..),
                     DeleteHdRoot (..), RestoreHdWallet,
                     UpdateHdRootPassword (..), UpdateHdWallet (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel, HdRoot,
                     WalletName, eskToHdRootId)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.Internal (PassiveWallet, walletKeystore,
                     wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core (getCurrentTimestamp)

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
    | UpdateWalletPasswordUnknownHdRoot HD.UnknownHdRoot
      -- ^ When trying to update the DB the input 'HdRootId' was not found.
    | UpdateWalletPasswordKeystoreChangedInTheMeantime HD.HdRootId
      -- ^ When trying to update the password inside the keystore, the
      -- previous 'PassPhrase' didn't match or it was deleted, which means
      -- this operation is not valid anymore.

instance Arbitrary UpdateWalletPasswordError where
    arbitrary = oneof []

instance Buildable UpdateWalletPasswordError where
    build (UpdateWalletPasswordOldPasswordMismatch hdRootId) =
        bprint ("UpdateWalletPasswordOldPasswordMismatch " % F.build) hdRootId
    build (UpdateWalletPasswordKeyNotFound hdRootId) =
        bprint ("UpdateWalletPasswordKeyNotFound " % F.build) hdRootId
    build (UpdateWalletPasswordUnknownHdRoot uRoot) =
        bprint ("UpdateWalletPasswordUnknownHdRoot " % F.build) uRoot
    build (UpdateWalletPasswordKeystoreChangedInTheMeantime uRoot) =
        bprint ("UpdateWalletPasswordKeystoreChangedInTheMeantime " % F.build) uRoot

instance Show UpdateWalletPasswordError where
    show = formatToString build

instance Exception UpdateWalletPasswordError

{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

-- | Creates a new HD 'Wallet'.
--
-- PRECONDITION: The input 'Mnemonic' should be supplied by the frontend such
-- that this is a brand new 'Mnemonic' never used before on the blockchain. For
-- other wallets restoration should be used.
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

    -- STEP 2: Insert the key into the keystore. We do this preemptively so that,
    -- in case of asynchronous exceptions, the worst which can happen is for the
    -- key to stay dangling into the keystore and the wallet not to be created.
    -- Then calling 'createHdWallet' a second time in this situation would
    -- correctly persist the wallet in the DB.
    -- The converse won't be true: leaving a dangling 'HdRoot' without its associated
    -- key in the keystore would break the sytem consistency and make the wallet
    -- unusable.
    let newRootId = eskToHdRootId esk
    Keystore.insert (WalletIdHdRnd newRootId) esk (pw ^. walletKeystore)

    -- STEP 3: Atomically generate the wallet and the initial internal structure in
    -- an acid-state transaction.
    res <- createWalletHdRnd pw
                             (spendingPassword /= emptyPassphrase)
                             walletName
                             assuranceLevel
                             esk
                             -- Brand new wallets have no Utxo
                             -- See preconditon above.
                             (\hdRoot -> Left $ CreateHdWallet hdRoot mempty)
    case res of
         Left e       -> return . Left $ CreateWalletFailed e
         Right hdRoot -> return (Right hdRoot)


-- | Creates an HD wallet where new accounts and addresses are generated
-- via random index derivation.
--
-- Fails with CreateHdWalletError if the HdRootId already exists.
createWalletHdRnd :: PassiveWallet
                  -> Bool
                  -- ^ Whether or not this wallet has a spending password set.
                  -> HD.WalletName
                  -> AssuranceLevel
                  -> EncryptedSecretKey
                  -> (HdRoot -> Either CreateHdWallet RestoreHdWallet)
                  -> IO (Either HD.CreateHdRootError HdRoot)
createWalletHdRnd pw hasSpendingPassword name assuranceLevel esk createWallet = do
    created <- InDb <$> getCurrentTimestamp
    let rootId  = eskToHdRootId esk
        newRoot = HD.initHdRoot rootId
                                name
                                (hdSpendingPassword created)
                                assuranceLevel
                                created

    res <- case createWallet newRoot of
             Left  create  -> update' (pw ^. wallets) create
             Right restore -> update' (pw ^. wallets) restore
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
            --
            -- NOTE ON ATOMICITY: In the case of asynchronous exceptions
            -- striking between STEP 1 & 2, note how this won't compromise the
            -- internal consistency of the system. Yes, it would leave a
            -- dangling key into the keystore, but that won't be as bad as
            -- trying to delete the key first @and then@ delete the wallet
            -- from the DB, which would expose us to consistency troubles as
            -- an 'HdRoot' without any associated keys in the keystore is
            -- unusable.
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
         Just oldKey -> do

             -- Predicate to check that the 2 password matches. It gets passed
             -- down to the 'Keystore' to ensure atomicity.
             let pwdCheck = maybe False (const True) . checkPassMatches oldPassword

             -- STEP 2: Compute the new key using the cryptographic primitives
             --         we have. This operation doesn't change any state, it
             --         just recomputes the new 'EncryptedSecretKey' in-place.
             genNewKey <- changeEncPassphrase oldPassword newPassword oldKey
             let mbNewKey = maybeToRight (UpdateWalletPasswordOldPasswordMismatch hdRootId) $
                            genNewKey

             case mbNewKey of
                  Left e -> return (Left e)
                  Right newKey -> do
                      -- STEP 3: Update the keystore, atomically.
                      swapped <- Keystore.compareAndReplace wId pwdCheck newKey keystore
                      case swapped of
                           -- We failed, the password changed in the
                           -- meantime, and the user needs to repeat the
                           -- operation.
                           Keystore.PredicateFailed ->
                               return $ Left (UpdateWalletPasswordOldPasswordMismatch hdRootId)
                           -- We failed, in the meantime the user deleted the
                           -- key.
                           Keystore.OldKeyLookupFailed -> do
                               return $ Left (UpdateWalletPasswordKeystoreChangedInTheMeantime hdRootId)
                           Keystore.Replaced -> do
                               -- STEP 4: Update the timestamp in the wallet data storage.
                               -- If we get interrupted here by an asynchronous exception the
                               -- price we will pay would be a slightly incorrect notion of
                               -- "how long ago we did change the password", but it won't
                               -- compromise the integrity of the system.
                               lastUpdateNow <- InDb <$> getCurrentTimestamp
                               let hasSpendingPassword = HD.HasSpendingPassword lastUpdateNow
                               res <- update' (pw ^. wallets)
                                              (UpdateHdRootPassword hdRootId hasSpendingPassword)
                               case res of
                                    Left e ->
                                        return $ Left (UpdateWalletPasswordUnknownHdRoot e)
                                    Right (db, hdRoot') -> return $ Right (db, hdRoot')
