module Cardano.Wallet.Kernel.Wallets (
    createHdWallet
    -- * Errors
    , CreateWalletError(..)
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
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, emptyPassphrase,
                     safeDeterministicKeyGen)
import           Pos.Txp.Toil (Utxo)

import           Cardano.Wallet.Kernel.BIP39 (Mnemonic)
import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import           Cardano.Wallet.Kernel.DB.AcidState (CreateHdWallet (..))
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

{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

-- | Creates a new HD 'Wallet'.
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
    res <- createWalletHdRnd pw spendingPassword walletName assuranceLevel esk mempty
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
                  -> PassPhrase
                  -> HD.WalletName
                  -> AssuranceLevel
                  -> EncryptedSecretKey
                  -> Utxo
                  -> IO (Either HD.CreateHdRootError HdRoot)
createWalletHdRnd pw spendingPassword name assuranceLevel esk utxo = do
    created <- InDb <$> getCurrentTimestamp
    let rootId        = eskToHdRootId esk
        newRoot = HD.initHdRoot rootId
                                name (hasSpendingPassword created)
                                assuranceLevel
                                created
        utxoByAccount = prefilterUtxo rootId esk utxo

    res <- update' (pw ^. wallets) $ CreateHdWallet newRoot utxoByAccount
    return $ either Left (const (Right newRoot)) res

    where

        -- | Whether or not the user decided to set a spending password for
        -- this wallet.
        hasSpendingPassword :: InDb Timestamp -> HD.HasSpendingPassword
        hasSpendingPassword created
            | spendingPassword == emptyPassphrase = HD.NoSpendingPassword
            | otherwise = HD.HasSpendingPassword created

