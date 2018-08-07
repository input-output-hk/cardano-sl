module Cardano.Wallet.WalletLayer.Types
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    -- * Getters
    , createWallet
    , getWalletIds
    , getWallet
    , updateWallet
    , deleteWallet

    , createAccount
    , getAccounts
    , getAccount
    , updateAccount
    , deleteAccount

    , createAddress
    , getAddresses
    , applyBlocks
    , rollbackBlocks
    -- * Errors
    , WalletLayerError(..)
    , CreateWalletError(..)
    , NewPaymentError(..)
    , EstimateFeesError(..)
    , CreateAddressError(..)
    , CreateAccountError(..)
    , GetAccountError(..)
    , GetAccountsError(..)
    , DeleteAccountError(..)
    , UpdateAccountError(..)
    ) where

import qualified Prelude
import           Universum

import           Control.Lens (makeLenses)

import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable

import           Cardano.Wallet.API.V1.Types (Account, AccountIndex,
                     AccountUpdate, Address, NewAccount, NewAddress, NewWallet,
                     Payment, V1 (..), Wallet, WalletId, WalletUpdate)

import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import qualified Cardano.Wallet.Kernel.DB.HdWallet as Kernel
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (TimeExecutionLimit)

import           Test.QuickCheck (Arbitrary (..), oneof)

import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (ExpenseRegulation, InputGrouping)

import           Pos.Chain.Block (Blund)
import           Pos.Core (Coin)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.Txp (Tx)
import           Pos.Crypto (PassPhrase)


------------------------------------------------------------
-- Errors creating a new Wallet
------------------------------------------------------------

data CreateWalletError =
      CreateWalletError Kernel.CreateWalletError
    | CreateWalletTimeLimitReached TimeExecutionLimit

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateWalletError where
    show = formatToString build

instance Exception CreateWalletError

instance Arbitrary CreateWalletError where
    arbitrary = oneof [ CreateWalletError <$> arbitrary
                      , CreateWalletTimeLimitReached <$> arbitrary
                      ]

instance Buildable CreateWalletError where
    build (CreateWalletError kernelError) =
        bprint ("CreateWalletError " % build) kernelError
    build (CreateWalletTimeLimitReached timeLimit) =
        bprint ("CreateWalletTimeLimitReached " % build) timeLimit

------------------------------------------------------------
-- Errors creating a new Address
------------------------------------------------------------

data CreateAddressError =
      CreateAddressError Kernel.CreateAddressError
    | CreateAddressAddressDecodingFailed Text
    -- ^ Decoding the input 'Text' as an 'Address' failed.
    | CreateAddressTimeLimitReached TimeExecutionLimit
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateAddressError where
    show = formatToString build

instance Exception CreateAddressError

instance Arbitrary CreateAddressError where
    arbitrary = oneof [ CreateAddressError <$> arbitrary
                      , pure (CreateAddressAddressDecodingFailed "Ae2tdPwUPEZ18ZjTLnLVr9CEvUEUX4eW1LBHbxxx")
                      ]

instance Buildable CreateAddressError where
    build (CreateAddressError kernelError) =
        bprint ("CreateAddressError " % build) kernelError
    build (CreateAddressAddressDecodingFailed txt) =
        bprint ("CreateAddressAddressDecodingFailed " % build) txt
    build (CreateAddressTimeLimitReached timeLimit) =
        bprint ("CreateAddressTimeLimitReached " % build) timeLimit

------------------------------------------------------------
-- Errors when dealing with Accounts
------------------------------------------------------------

data CreateAccountError =
      CreateAccountError Kernel.CreateAccountError
    | CreateAccountWalletIdDecodingFailed Text
    -- ^ Decoding the parent's 'WalletId' from a raw 'Text' failed.
    | CreateAccountTimeLimitReached TimeExecutionLimit
    | CreateAccountFirstAddressGenerationFailed Kernel.CreateAddressError
    -- ^ When trying to create the first 'Address' to go in tandem with this
    -- 'Account', the generation failed.
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateAccountError where
    show = formatToString build

instance Exception CreateAccountError

instance Buildable CreateAccountError where
    build (CreateAccountError kernelError) =
        bprint ("CreateAccountError " % build) kernelError
    build (CreateAccountWalletIdDecodingFailed txt) =
        bprint ("CreateAccountWalletIdDecodingFailed " % build) txt
    build (CreateAccountTimeLimitReached timeLimit) =
        bprint ("CreateAccountTimeLimitReached " % build) timeLimit
    build (CreateAccountFirstAddressGenerationFailed kernelError) =
        bprint ("CreateAccountFirstAddressGenerationFailed " % build) kernelError

data GetAccountError =
      GetAccountError (V1 Kernel.UnknownHdAccount)
    | GetAccountWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetAccountError where
    show = formatToString build

instance Exception GetAccountError

instance Buildable GetAccountError where
    build (GetAccountError kernelError) =
        bprint ("GetAccountError " % build) kernelError
    build (GetAccountWalletIdDecodingFailed txt) =
        bprint ("GetAccountWalletIdDecodingFailed " % build) txt

data DeleteAccountError =
      DeleteAccountError (V1 Kernel.UnknownHdAccount)
    | DeleteAccountWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show DeleteAccountError where
    show = formatToString build

instance Exception DeleteAccountError

instance Buildable DeleteAccountError where
    build (DeleteAccountError kernelError) =
        bprint ("DeleteAccountError " % build) kernelError
    build (DeleteAccountWalletIdDecodingFailed txt) =
        bprint ("DeleteAccountWalletIdDecodingFailed " % build) txt

data GetAccountsError =
      GetAccountsError Kernel.UnknownHdRoot
    | GetAccountsWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetAccountsError where
    show = formatToString build

instance Exception GetAccountsError

instance Buildable GetAccountsError where
    build (GetAccountsError kernelError) =
        bprint ("GetAccountsError " % build) kernelError
    build (GetAccountsWalletIdDecodingFailed txt) =
        bprint ("GetAccountsWalletIdDecodingFailed " % build) txt

data UpdateAccountError =
      UpdateAccountError (V1 Kernel.UnknownHdAccount)
    | UpdateAccountWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateAccountError where
    show = formatToString build

instance Exception UpdateAccountError

instance Buildable UpdateAccountError where
    build (UpdateAccountError kernelError) =
        bprint ("UpdateAccountError " % build) kernelError
    build (UpdateAccountWalletIdDecodingFailed txt) =
        bprint ("UpdateAccountWalletIdDecodingFailed " % build) txt

------------------------------------------------------------
-- General-purpose errors which may arise when working with
-- the wallet layer
------------------------------------------------------------

data WalletLayerError =
    InvalidAddressConversionFailed Text
    -- ^ Trying to decode the input 'Text' into a Cardano 'Address' failed
    deriving Show

instance Exception WalletLayerError

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- * wallets
      _pwlCreateWallet   :: NewWallet -> m (Either CreateWalletError Wallet)
    , _pwlGetWalletIds   :: m [WalletId]
    , _pwlGetWallet      :: WalletId -> m (Maybe Wallet)
    , _pwlUpdateWallet   :: WalletId -> WalletUpdate -> m Wallet
    , _pwlDeleteWallet   :: WalletId -> m Bool
    -- * accounts
    , _pwlCreateAccount  :: WalletId
                         -> NewAccount
                         -> m (Either CreateAccountError Account)
    , _pwlGetAccounts    :: WalletId -> m (Either GetAccountsError (IxSet Account))
    , _pwlGetAccount     :: WalletId
                         -> AccountIndex
                         -> m (Either GetAccountError Account)
    , _pwlUpdateAccount  :: WalletId
                         -> AccountIndex
                         -> AccountUpdate
                         -> m (Either UpdateAccountError Account)
    , _pwlDeleteAccount  :: WalletId
                         -> AccountIndex
                         -> m (Either DeleteAccountError ())
    -- * addresses
    , _pwlCreateAddress  :: NewAddress -> m (Either CreateAddressError Address)
    , _pwlGetAddresses   :: WalletId -> m [Address]
    -- * core API
    , _pwlApplyBlocks    :: OldestFirst NE Blund -> m ()
    , _pwlRollbackBlocks :: NewestFirst NE Blund -> m ()
    }

makeLenses ''PassiveWalletLayer

------------------------------------------------------------
-- Passive wallet layer getters
------------------------------------------------------------

createWallet :: forall m. PassiveWalletLayer m
             -> NewWallet
             -> m (Either CreateWalletError Wallet)
createWallet pwl = pwl ^. pwlCreateWallet

getWalletIds :: forall m. PassiveWalletLayer m -> m [WalletId]
getWalletIds pwl = pwl ^. pwlGetWalletIds

getWallet :: forall m. PassiveWalletLayer m -> WalletId -> m (Maybe Wallet)
getWallet pwl = pwl ^. pwlGetWallet

updateWallet :: forall m. PassiveWalletLayer m -> WalletId -> WalletUpdate -> m Wallet
updateWallet pwl = pwl ^. pwlUpdateWallet

deleteWallet :: forall m. PassiveWalletLayer m -> WalletId -> m Bool
deleteWallet pwl = pwl ^. pwlDeleteWallet


createAccount :: forall m. PassiveWalletLayer m
              -> WalletId
              -> NewAccount
              -> m (Either CreateAccountError Account)
createAccount pwl = pwl ^. pwlCreateAccount

getAccounts :: forall m. PassiveWalletLayer m
            -> WalletId
            -> m (Either GetAccountsError (IxSet Account))
getAccounts pwl = pwl ^. pwlGetAccounts

getAccount :: forall m. PassiveWalletLayer m
           -> WalletId
           -> AccountIndex
           -> m (Either GetAccountError Account)
getAccount pwl = pwl ^. pwlGetAccount

updateAccount :: forall m. PassiveWalletLayer m
              -> WalletId
              -> AccountIndex
              -> AccountUpdate
              -> m (Either UpdateAccountError Account)
updateAccount pwl = pwl ^. pwlUpdateAccount

deleteAccount :: forall m. PassiveWalletLayer m
              -> WalletId
              -> AccountIndex
              -> m (Either DeleteAccountError ())
deleteAccount pwl = pwl ^. pwlDeleteAccount

createAddress :: forall m. PassiveWalletLayer m -> NewAddress -> m (Either CreateAddressError Address)
createAddress pwl = pwl ^. pwlCreateAddress

getAddresses :: forall m. PassiveWalletLayer m -> WalletId -> m [Address]
getAddresses pwl = pwl ^. pwlGetAddresses


applyBlocks :: forall m. PassiveWalletLayer m -> OldestFirst NE Blund -> m ()
applyBlocks pwl = pwl ^. pwlApplyBlocks

rollbackBlocks :: forall m. PassiveWalletLayer m -> NewestFirst NE Blund -> m ()
rollbackBlocks pwl = pwl ^. pwlRollbackBlocks

------------------------------------------------------------
-- Active wallet layer
------------------------------------------------------------

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | Performs a payment.
    , pay :: PassPhrase
          -- ^ The \"spending password\" to decrypt the 'EncryptedSecretKey'.
          -> InputGrouping
          -- ^ An preference on how to group inputs during coin selection.
          -> ExpenseRegulation
          -- ^ Who pays the fee, if the sender or the receivers.
          -> Payment
          -- ^ The payment we need to perform.
          -> m (Either NewPaymentError Tx)
      -- | Estimates the fees for a payment.
    , estimateFees   :: PassPhrase
                     -- ^ The \"spending password\" to decrypt the 'EncryptedSecretKey'.
                     -> InputGrouping
                     -- ^ An preference on how to group inputs during coin selection
                     -> ExpenseRegulation
                     -- ^ Who pays the fee, if the sender or the receivers.
                     -> Payment
                     -- ^ The payment we need to perform.
                     -> m (Either EstimateFeesError Coin)
    }

------------------------------------------------------------
-- Active wallet errors
------------------------------------------------------------

data NewPaymentError =
      NewPaymentError Kernel.PaymentError
    | NewPaymentTimeLimitReached TimeExecutionLimit

-- | Unsound show instance needed for the 'Exception' instance.
instance Show NewPaymentError where
    show = formatToString build

instance Exception NewPaymentError

instance Buildable NewPaymentError where
    build (NewPaymentError kernelErr) =
        bprint ("NewPaymentError " % build) kernelErr
    build (NewPaymentTimeLimitReached ter) =
        bprint ("NewPaymentTimeLimitReached " % build) ter

data EstimateFeesError =
      EstimateFeesError Kernel.EstimateFeesError
    | EstimateFeesTimeLimitReached TimeExecutionLimit

-- | Unsound show instance needed for the 'Exception' instance.
instance Show EstimateFeesError where
    show = formatToString build

instance Exception EstimateFeesError

instance Buildable EstimateFeesError where
    build (EstimateFeesError kernelErr) =
        bprint ("EstimateFeesError " % build) kernelErr
    build (EstimateFeesTimeLimitReached ter) =
        bprint ("EstimateFeesTimeLimitReached " % build) ter

instance Arbitrary EstimateFeesError where
    arbitrary = oneof [ EstimateFeesError <$> arbitrary
                      , EstimateFeesTimeLimitReached <$> arbitrary
                      ]
