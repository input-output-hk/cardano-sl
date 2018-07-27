module Cardano.Wallet.Kernel.Accounts (
      createAccount
    , deleteAccount
    , updateAccount
    -- * Errors
    , CreateAccountError(..)
    , DeleteAccountError(..)
    , UpdateAccountError(..)
    ) where

import qualified Prelude
import           Universum

import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F
import qualified Formatting.Buildable
import           System.Random.MWC (GenIO, createSystemRandom, uniformR)

import           Data.Acid (update)

import           Pos.Core (mkCoin)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase)

import           Cardano.Wallet.Kernel.DB.AcidState (CreateHdAccount (..), DB,
                     DeleteHdAccount (..), UpdateHdAccountName (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (AccountName (..),
                     HdAccount (..), HdAccountId (..), HdAccountIx (..),
                     HdRootId, hdAccountIdParent, hdAccountName)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create
                     (CreateHdAccountError (..), initHdAccount)
import           Cardano.Wallet.Kernel.DB.HdWallet.Delete
                     (DeleteHdAccountError (..))
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.HdWallet.Update
                     (UpdateHdAccountError (..))
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.Spec (Checkpoint (..), emptyPending)
import           Cardano.Wallet.Kernel.Internal (PassiveWallet, walletKeystore,
                     wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (WalletId (..))

data CreateAccountError =
      CreateAccountUnknownHdRoot HdRootId
      -- ^ When trying to create the 'Account', the parent 'HdRoot' was not
      -- there.
    | CreateAccountKeystoreNotFound WalletId
      -- ^ When trying to create the 'Account', the 'Keystore' didn't have
      -- any secret associated with the input 'WalletId'.
    | CreateAccountHdRndAccountSpaceSaturated HdRootId
      -- ^ The available number of HD accounts in use is such that trying
      -- to find another random index would be too expensive.
    deriving Eq

instance Buildable CreateAccountError where
    build (CreateAccountUnknownHdRoot uRoot) =
        bprint ("CreateAccountUnknownHdRoot " % F.build) uRoot
    build (CreateAccountKeystoreNotFound accId) =
        bprint ("CreateAccountKeystoreNotFound " % F.build) accId
    build (CreateAccountHdRndAccountSpaceSaturated hdAcc) =
        bprint ("CreateAccountHdRndAccountSpaceSaturated " % F.build) hdAcc

instance Show CreateAccountError where
    show = formatToString build

instance Exception CreateAccountError

--
--  Delete-related errors
--

data DeleteAccountError =
      DeleteAccountUnknownHdRoot HdRootId
      -- ^ When trying to delete the 'Account', the parent 'HdRoot' was not
      -- there.
    deriving Eq

instance Buildable DeleteAccountError where
    build (DeleteAccountUnknownHdRoot uRoot) =
        bprint ("DeleteAccountUnknownHdRoot " % F.build) uRoot

instance Show DeleteAccountError where
    show = formatToString build

instance Exception DeleteAccountError

--
-- Update-related errors

data UpdateAccountError =
      UpdateAccountUnknownHdRoot HdRootId
      -- ^ When trying to update the 'Account', the parent 'HdRoot' was not
      -- there.
    | UpdateAccountUnknownHdAccount HdAccountId
      -- ^ When trying to update the 'Account', the account was not there.
    deriving Eq

instance Buildable UpdateAccountError where
    build (UpdateAccountUnknownHdRoot uRoot) =
        bprint ("UpdateAccountUnknownHdRoot " % F.build) uRoot
    build (UpdateAccountUnknownHdAccount uAccount) =
        bprint ("UpdateAccountUnknownHdAccount " % F.build) uAccount

instance Show UpdateAccountError where
    show = formatToString build

instance Exception UpdateAccountError

-- | Creates a new 'Account' for the input wallet.
-- Note: @it does not@ generate a new 'Address' to go in tandem with this
-- 'Account'. This will be responsibility of the wallet layer.
createAccount :: PassPhrase
              -- ^ The 'Passphrase' (a.k.a the \"Spending Password\").
              -> AccountName
              -- ^ The name for this account.
              -> WalletId
              -- ^ An abstract notion of a 'Wallet identifier
              -> PassiveWallet
              -> IO (Either CreateAccountError HdAccount)
createAccount spendingPassword accountName walletId pw = do
    let keystore = pw ^. walletKeystore
    case walletId of
         (WalletIdHdRnd hdRootId) -> do
             mbEsk <- Keystore.lookup (WalletIdHdRnd hdRootId) keystore
             case mbEsk of
                  Nothing  -> return (Left $ CreateAccountKeystoreNotFound walletId)
                  Just esk ->
                      createHdRndAccount spendingPassword
                                         accountName
                                         esk
                                         hdRootId
                                         pw

-- | Creates a new 'Account' using the random HD derivation under the hood.
-- This code follows the same pattern of 'createHdRndAddress', but the two
-- functions are "similarly different" enough to not make convenient generalise
-- the code.
createHdRndAccount :: PassPhrase
                   -> AccountName
                   -> EncryptedSecretKey
                   -> HdRootId
                   -> PassiveWallet
                   -> IO (Either CreateAccountError HdAccount)
createHdRndAccount _spendingPassword accountName _esk rootId pw = do
    gen <- createSystemRandom
    go gen 0
    where
        go :: GenIO -> Word32 -> IO (Either CreateAccountError HdAccount)
        go gen collisions =
            case collisions >= maxAllowedCollisions of
                 True  -> return $ Left (CreateAccountHdRndAccountSpaceSaturated rootId)
                 False -> tryGenerateAccount gen collisions

        tryGenerateAccount :: GenIO
                           -> Word32
                           -- ^ The current number of collisions
                           -> IO (Either CreateAccountError HdAccount)
        tryGenerateAccount gen collisions = do
            newIndex <- deriveIndex (flip uniformR gen) HdAccountIx HardDerivation
            let hdAccountId = HdAccountId rootId newIndex
                newAccount  = initHdAccount hdAccountId genesisCheckpoint &
                              hdAccountName .~ accountName
                db = pw ^. wallets
            res <- update db (CreateHdAccount newAccount)
            case res of
                 (Left (CreateHdAccountExists _)) ->
                     go gen (succ collisions)
                 (Left (CreateHdAccountUnknownRoot _)) ->
                     return (Left $ CreateAccountUnknownHdRoot rootId)
                 Right () -> return (Right newAccount)

        -- The maximum number of allowed collisions.
        maxAllowedCollisions :: Word32
        maxAllowedCollisions = 42

        genesisCheckpoint :: Checkpoint
        genesisCheckpoint = Checkpoint {
              _checkpointUtxo        = InDb mempty
            , _checkpointUtxoBalance = InDb (mkCoin 0)
            , _checkpointPending     = emptyPending
            , _checkpointBlockMeta   = mempty
            }


-- | Deletes an HD 'Account' from the data storage.
deleteAccount :: HdAccountId -> PassiveWallet -> IO (Either DeleteAccountError ())
deleteAccount hdAccountId pw = do
    res <- liftIO $ update (pw ^. wallets) (DeleteHdAccount hdAccountId)
    return $ case res of
         Left (DeleteHdAccountUnknownRoot _unknownRoot) ->
             Left (DeleteAccountUnknownHdRoot (hdAccountId ^. hdAccountIdParent))
         Right () -> Right ()

-- | Updates an HD 'Account'.
updateAccount :: HdAccountId
              -> AccountName
              -- ^ The new name for this account.
              -> PassiveWallet
              -> IO (Either UpdateAccountError (DB, HdAccount))
updateAccount hdAccountId newAccountName pw = do
    res <- liftIO $ update (pw ^. wallets) (UpdateHdAccountName hdAccountId newAccountName)
    return $ case res of
         Left (UpdateHdAccountUnknownRoot _unknownRoot) ->
             Left (UpdateAccountUnknownHdRoot (hdAccountId ^. hdAccountIdParent))
         Left (UpdateHdAccountUnknownAccount _unknownAccount) ->
             Left (UpdateAccountUnknownHdAccount hdAccountId)
         Right (db, account) -> Right (db, account)
