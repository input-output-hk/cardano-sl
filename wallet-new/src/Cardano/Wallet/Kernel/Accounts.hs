module Cardano.Wallet.Kernel.Accounts (
      createHdRandomAccount
    , createHdFixedAccount
    , deleteAccount
    , updateAccount
    -- * Errors
    , CreateAccountError(..)
    ) where

import qualified Prelude
import           Universum

import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F
import qualified Formatting.Buildable
import           System.Random.MWC (GenIO, createSystemRandom, uniformR)

import           Data.Acid (update)

import           Cardano.Wallet.Kernel.DB.AcidState (CreateHdAccount (..), DB,
                     DeleteHdAccount (..), UpdateHdAccountName (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (AccountName (..),
                     HdAccount (..), HdAccountId (..), HdAccountIx (..),
                     HdAccountState (..), HdAccountUpToDate (..), HdRootId,
                     UnknownHdAccount (..), hdAccountName)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create
                     (CreateHdAccountError (..), initHdAccount)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.Spec (Checkpoints (..),
                     initCheckpoint)
import           Cardano.Wallet.Kernel.Internal (PassiveWallet, walletKeystore,
                     wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (WalletId (..))

import           Test.QuickCheck (Arbitrary (..), oneof)

data CreateAccountError =
      CreateAccountUnknownHdRoot HdRootId
      -- ^ When trying to create the 'Account', the parent 'HdRoot' was not
      -- there.
    | CreateAccountKeystoreNotFound WalletId
      -- ^ When trying to create the 'Account', the 'Keystore' didn't have
      -- any secret associated with the input 'WalletId'.
    | CreateAccountAlreadyExists HdRootId HdAccountIx
      -- ^ When creating a certain account, the account already existed in
      -- the database.
    | CreateAccountHdRndAccountSpaceSaturated HdRootId
      -- ^ The available number of HD accounts in use is such that trying
      -- to find another random index would be too expensive.
    deriving Eq

instance Arbitrary CreateAccountError where
    arbitrary = oneof []

instance Buildable CreateAccountError where
    build (CreateAccountUnknownHdRoot uRoot) =
        bprint ("CreateAccountUnknownHdRoot " % F.build) uRoot
    build (CreateAccountKeystoreNotFound accId) =
        bprint ("CreateAccountKeystoreNotFound " % F.build) accId
    build (CreateAccountAlreadyExists rootId accId) =
        bprint ("CreateAccountAlreadyExists " % F.build % F.build) rootId accId
    build (CreateAccountHdRndAccountSpaceSaturated hdAcc) =
        bprint ("CreateAccountHdRndAccountSpaceSaturated " % F.build) hdAcc

instance Show CreateAccountError where
    show = formatToString build

instance Exception CreateAccountError


createHdRandomAccount :: AccountName
                      -- ^ The name for this account.
                      -> WalletId
                      -- ^ An abstract notion of a 'Wallet identifier
                      -> PassiveWallet
                      -> IO (Either CreateAccountError HdAccount)
createHdRandomAccount = createAccount newHdRndAccount

-- | Creates an 'HdAccount' from a @fixed@ index. Note how, despite tempting,
-- it's incorrect to call this @sequential generation@, as in such case the
-- index wouldn't be passed but merely an internal detail of such function.
-- @fixed@ here really means "externally provided".
createHdFixedAccount :: HdAccountIx
                     -- ^ The account index to target
                     -> AccountName
                     -- ^ The name for this account.
                     -> WalletId
                     -- ^ An abstract notion of a 'Wallet identifier
                     -> PassiveWallet
                     -> IO (Either CreateAccountError HdAccount)
createHdFixedAccount newIndex = createAccount (newHdFixedAccount newIndex)


type MkNewAccount = AccountName
                  -> HdRootId
                  -> PassiveWallet
                  -> IO (Either CreateAccountError HdAccount)

-- | Creates a new 'Account' for the input wallet.
-- Note: @it does not@ generate a new 'Address' to go in tandem with this
-- 'Account'. This will be responsibility of the caller.
createAccount :: MkNewAccount
              -- ^ A function to create the account.
              -> AccountName
              -- ^ The name for this account.
              -> WalletId
              -- ^ An abstract notion of a 'Wallet identifier
              -> PassiveWallet
              -> IO (Either CreateAccountError HdAccount)
createAccount creationFunction accountName walletId pw = do
    let keystore = pw ^. walletKeystore
    case walletId of
         WalletIdHdRnd hdRootId -> do
             mbEsk <- Keystore.lookup (WalletIdHdRnd hdRootId) keystore
             case mbEsk of
                  Nothing  -> return (Left $ CreateAccountKeystoreNotFound walletId)
                  Just _   -> creationFunction accountName hdRootId pw

-- | Creates a new 'Account' using a fixed (given) index.
newHdFixedAccount :: HdAccountIx
                  -> AccountName
                  -> HdRootId
                  -> PassiveWallet
                  -> IO (Either CreateAccountError HdAccount)
newHdFixedAccount newIndex accountName rootId pw = do
    let onFailure err = case err of
         CreateHdAccountExists _ ->
             -- Nothing we can do; we were asked to create a specific account
             -- with a specific index, but there was a collision in the DB
             return (Left $ CreateAccountAlreadyExists rootId newIndex)
         CreateHdAccountUnknownRoot _ ->
             return (Left $ CreateAccountUnknownHdRoot rootId)
    tryGenerateAccount onFailure newIndex rootId accountName pw

-- | Creates a new 'Account' using the random HD derivation under the hood.
-- This code follows the same pattern of 'createHdRndAddress', but the two
-- functions are "similarly different" enough to not make convenient generalise
-- the code.
newHdRndAccount :: AccountName
                -> HdRootId
                -> PassiveWallet
                -> IO (Either CreateAccountError HdAccount)
newHdRndAccount accountName rootId pw = do
    gen <- createSystemRandom
    go gen 0
    where
        go :: GenIO -> Word32 -> IO (Either CreateAccountError HdAccount)
        go gen collisions =
            case collisions >= maxAllowedCollisions of
                 True  -> return $ Left (CreateAccountHdRndAccountSpaceSaturated rootId)
                 False -> do
                     let onFailure err = case err of
                           CreateHdAccountExists _ ->
                               go gen (succ collisions)
                           CreateHdAccountUnknownRoot _ ->
                               return (Left $ CreateAccountUnknownHdRoot rootId)
                     newIndex <- deriveIndex (flip uniformR gen) HdAccountIx HardDerivation
                     tryGenerateAccount onFailure newIndex rootId accountName pw

        -- The maximum number of allowed collisions. This number was
        -- empirically calculated based on a [beta distribution](https://en.wikipedia.org/wiki/Beta_distribution).
        -- In particular, it can be shown how even picking small values for
        -- @alpha@ and @beta@, the probability of failing after the next
        -- collision rapidly approaches 99%. With 50 attempts, our probability
        -- to fail is 98%, and the 42 is a nice easter egg very close to 50,
        -- this is why it was picked.
        maxAllowedCollisions :: Word32
        maxAllowedCollisions = 42

tryGenerateAccount :: (CreateHdAccountError -> IO (Either CreateAccountError HdAccount))
                   -- ^ An action to be run in case of errors
                   -> HdAccountIx
                   -> HdRootId
                   -> AccountName
                   -- ^ The requested index
                   -> PassiveWallet
                   -> IO (Either CreateAccountError HdAccount)
tryGenerateAccount onFailure newIndex rootId accountName pw = do
    let hdAccountId = HdAccountId rootId newIndex
        newAccount  = initHdAccount hdAccountId initialAccountState &
                      hdAccountName .~ accountName
        db = pw ^. wallets
    res <- update db (CreateHdAccount newAccount)
    case res of
         (Left e) -> onFailure e
         Right () -> return (Right newAccount)
  where
    initialAccountState :: HdAccountState
    initialAccountState = HdAccountStateUpToDate HdAccountUpToDate {
          _hdUpToDateCheckpoints = Checkpoints . one $ initCheckpoint mempty
        }



-- | Deletes an HD 'Account' from the data storage.
deleteAccount :: HdAccountId
              -> PassiveWallet
              -> IO (Either UnknownHdAccount ())
deleteAccount hdAccountId pw = do
    res <- liftIO $ update (pw ^. wallets) (DeleteHdAccount hdAccountId)
    return $ case res of
         Left dbErr -> Left dbErr
         Right ()   -> Right ()

-- | Updates an HD 'Account'.
updateAccount :: HdAccountId
              -> AccountName
              -- ^ The new name for this account.
              -> PassiveWallet
              -> IO (Either UnknownHdAccount (DB, HdAccount))
updateAccount hdAccountId newAccountName pw = do
    res <- liftIO $ update (pw ^. wallets) (UpdateHdAccountName hdAccountId newAccountName)
    return $ case res of
         Left dbError        -> Left dbError
         Right (db, account) -> Right (db, account)
