module Cardano.Wallet.WalletLayer
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    -- * Getters
    , createWallet
    , getWallets
    , getWallet
    , updateWallet
    , updateWalletPassword
    , deleteWallet

    , createAccount
    , getAccounts
    , getAccount
    , getAccountBalance
    , getAccountAddresses
    , updateAccount
    , deleteAccount

    , createAddress
    , getAddresses
    , validateAddress

    , getTransactions
    , getTxFromMeta

    , applyBlocks
    , rollbackBlocks
    -- * Errors
    , CreateWalletError(..)
    , GetWalletError(..)
    , UpdateWalletError(..)
    , UpdateWalletPasswordError(..)
    , DeleteWalletError(..)
    , NewPaymentError(..)
    , EstimateFeesError(..)
    , RedeemAdaError(..)
    , CreateAddressError(..)
    , ValidateAddressError(..)
    , CreateAccountError(..)
    , GetAccountError(..)
    , GetAccountsError(..)
    , GetTxError(..)
    , DeleteAccountError(..)
    , UpdateAccountError(..)
    ) where

import           Universum

import           Control.Lens (makeLenses)
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import qualified Prelude
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Pos.Chain.Block (Blund)
import           Pos.Core (Coin, Timestamp)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.Txp (Tx, TxId)
import           Pos.Crypto (PassPhrase)

import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
import           Cardano.Wallet.API.Request.Sort (SortOperations (..))
import           Cardano.Wallet.API.Response (SliceOf (..), WalletResponse)
import           Cardano.Wallet.API.V1.Types (Account, AccountBalance,
                     AccountIndex, AccountUpdate, Address, NewAccount,
                     NewAddress, NewWallet, PasswordUpdate, Payment,
                     Redemption, Transaction, V1 (..), Wallet, WalletAddress,
                     WalletId, WalletUpdate)
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (ExpenseRegulation, InputGrouping)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as Kernel
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (TimeExecutionLimit)

------------------------------------------------------------
-- Errors when manipulating wallets
------------------------------------------------------------

data CreateWalletError =
      CreateWalletError Kernel.CreateWalletError
    | CreateWalletFirstAccountCreationFailed Kernel.CreateAccountError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateWalletError where
    show = formatToString build

instance Exception CreateWalletError

instance Arbitrary CreateWalletError where
    arbitrary = oneof [ CreateWalletError <$> arbitrary
                      , CreateWalletFirstAccountCreationFailed <$> arbitrary
                      ]

instance Buildable CreateWalletError where
    build (CreateWalletError kernelError) =
        bprint ("CreateWalletError " % build) kernelError
    build (CreateWalletFirstAccountCreationFailed kernelError) =
        bprint ("CreateWalletFirstAccountCreationFailed " % build) kernelError

data GetWalletError =
      GetWalletError (V1 Kernel.UnknownHdRoot)
    | GetWalletErrorNotFound WalletId
    -- ^ Error thrown by the legacy wallet layer, isomorphic to the one above,
    -- which is new-data-layer specific.
    | GetWalletWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetWalletError where
    show = formatToString build

instance Exception GetWalletError

instance Buildable GetWalletError where
    build (GetWalletError (V1 kernelError)) =
        bprint ("GetWalletError " % build) kernelError
    build (GetWalletErrorNotFound walletId) =
        bprint ("GetWalletErrorNotFound " % build) walletId
    build (GetWalletWalletIdDecodingFailed txt) =
        bprint ("GetWalletWalletIdDecodingFailed " % build) txt

data UpdateWalletError =
      UpdateWalletError (V1 Kernel.UnknownHdRoot)
    | UpdateWalletErrorNotFound WalletId
    -- ^ Error thrown by the legacy wallet layer, isomorphic to the one above,
    -- which is new-data-layer specific.
    | UpdateWalletWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateWalletError where
    show = formatToString build

instance Exception UpdateWalletError

instance Buildable UpdateWalletError where
    build (UpdateWalletError (V1 kernelError)) =
        bprint ("UpdateWalletError " % build) kernelError
    build (UpdateWalletErrorNotFound walletId) =
        bprint ("UpdateWalletErrorNotFound " % build) walletId
    build (UpdateWalletWalletIdDecodingFailed txt) =
        bprint ("UpdateWalletWalletIdDecodingFailed " % build) txt

data UpdateWalletPasswordError =
      UpdateWalletPasswordWalletIdDecodingFailed Text
    | UpdateWalletPasswordError Kernel.UpdateWalletPasswordError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateWalletPasswordError where
    show = formatToString build

instance Exception UpdateWalletPasswordError

instance Buildable UpdateWalletPasswordError where
    build (UpdateWalletPasswordWalletIdDecodingFailed txt) =
        bprint ("UpdateWalletPasswordWalletIdDecodingFailed " % build) txt
    build (UpdateWalletPasswordError kernelError) =
        bprint ("UpdateWalletPasswordError " % build) kernelError

data DeleteWalletError =
      DeleteWalletWalletIdDecodingFailed Text
    | DeleteWalletError (V1 Kernel.UnknownHdRoot)

-- | Unsound show instance needed for the 'Exception' instance.
instance Show DeleteWalletError where
    show = formatToString build

instance Exception DeleteWalletError

instance Buildable DeleteWalletError where
    build (DeleteWalletWalletIdDecodingFailed txt) =
        bprint ("DeleteWalletWalletIdDecodingFailed " % build) txt
    build (DeleteWalletError kernelError) =
        bprint ("DeleteWalletError " % build) kernelError

------------------------------------------------------------
-- Errors when dealing with addresses
------------------------------------------------------------

data CreateAddressError =
      CreateAddressError Kernel.CreateAddressError
    | CreateAddressAddressDecodingFailed Text
    -- ^ Decoding the input 'Text' as an 'Address' failed.
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

data ValidateAddressError =
      ValidateAddressDecodingFailed Text
    -- ^ When trying to decode this raw 'Text' into a proper Cardano
    -- 'Address' the decoding failed. Unfortunately we are not able to
    -- provide a more accurate error description as 'decodeTextAddress' doesn't
    -- offer such.
    | ValidateAddressNotOurs Address
    -- ^ The input address is a valid 'Cardano' address, but it doesn't
    -- belong to us.
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show ValidateAddressError where
    show = formatToString build

instance Exception ValidateAddressError

instance Buildable ValidateAddressError where
    build (ValidateAddressDecodingFailed rawText) =
        bprint ("ValidateAddressDecodingFailed " % build) rawText
    build (ValidateAddressNotOurs address) =
        bprint ("ValidateAddressNotOurs " % build) address

------------------------------------------------------------
-- Errors when dealing with Accounts
------------------------------------------------------------

data CreateAccountError =
      CreateAccountError Kernel.CreateAccountError
    | CreateAccountWalletIdDecodingFailed Text
    -- ^ Decoding the parent's 'WalletId' from a raw 'Text' failed.
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
-- Errors when getting Transactions
------------------------------------------------------------

data GetTxError =
      GetTxMissingWalletIdError
    | GetTxAddressDecodingFailed Text
    | GetTxInvalidSortingOperaration String
    | GetTxUnknownHdAccount Kernel.UnknownHdAccount

instance Show GetTxError where
    show = formatToString build

instance Buildable GetTxError where
    build GetTxMissingWalletIdError =
        bprint "GetTxMissingWalletIdError "
    build (GetTxAddressDecodingFailed txt) =
        bprint ("GetTxAddressDecodingFailed " % build) txt
    build (GetTxInvalidSortingOperaration txt) =
        bprint ("GetTxInvalidSortingOperaration " % build) txt
    build (GetTxUnknownHdAccount err) =
        bprint ("GetTxUnknownHdAccount " % build) err


instance Arbitrary GetTxError where
    arbitrary = oneof [ pure GetTxMissingWalletIdError
                      , pure (GetTxAddressDecodingFailed "by_amount")
                      , pure (GetTxInvalidSortingOperaration "123")
                      , GetTxUnknownHdAccount <$> arbitrary
                      ]

instance Exception GetTxError

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- wallets
      _pwlCreateWallet         :: NewWallet -> m (Either CreateWalletError Wallet)
    , _pwlGetWallets           :: m (IxSet Wallet)
    , _pwlGetWallet            :: WalletId -> m (Either GetWalletError Wallet)
    , _pwlUpdateWallet         :: WalletId
                               -> WalletUpdate
                               -> m (Either UpdateWalletError Wallet)
    , _pwlUpdateWalletPassword :: WalletId
                               -> PasswordUpdate
                               -> m (Either UpdateWalletPasswordError Wallet)
    , _pwlDeleteWallet         :: WalletId -> m (Either DeleteWalletError ())
    -- accounts
    , _pwlCreateAccount        :: WalletId
                               -> NewAccount
                               -> m (Either CreateAccountError Account)
    , _pwlGetAccounts          :: WalletId
                               -> m (Either GetAccountsError (IxSet Account))
    , _pwlGetAccount           :: WalletId
                               -> AccountIndex
                               -> m (Either GetAccountError Account)
    , _pwlGetAccountBalance    :: WalletId
                               -> AccountIndex
                               -> m (Either GetAccountError AccountBalance)
    , _pwlGetAccountAddresses  :: WalletId
                               -> AccountIndex
                               -> RequestParams
                               -> FilterOperations '[V1 Address] WalletAddress
                               -> m (Either GetAccountError (WalletResponse [WalletAddress]))
    , _pwlUpdateAccount        :: WalletId
                               -> AccountIndex
                               -> AccountUpdate
                               -> m (Either UpdateAccountError Account)
    , _pwlDeleteAccount        :: WalletId
                               -> AccountIndex
                               -> m (Either DeleteAccountError ())
    -- addresses
    , _pwlCreateAddress        :: NewAddress
                               -> m (Either CreateAddressError WalletAddress)
    , _pwlGetAddresses         :: RequestParams -> m (SliceOf WalletAddress)
    , _pwlValidateAddress      :: Text
                               -> m (Either ValidateAddressError WalletAddress)

    -- transactions
    , _pwlGetTransactions      :: Maybe WalletId -> Maybe AccountIndex -> Maybe (V1 Address)
        -> RequestParams -> FilterOperations '[V1 TxId, V1 Timestamp] Transaction -> SortOperations Transaction -> m (Either GetTxError (WalletResponse [Transaction]))
    , _pwlGetTxFromMeta        :: TxMeta -> m (Either Kernel.UnknownHdAccount Transaction)

    -- core API
    , _pwlApplyBlocks          :: OldestFirst NE Blund -> m ()
    , _pwlRollbackBlocks       :: NewestFirst NE Blund -> m ()
    }

makeLenses ''PassiveWalletLayer

------------------------------------------------------------
-- Passive wallet layer getters
------------------------------------------------------------

createWallet :: forall m. PassiveWalletLayer m
             -> NewWallet
             -> m (Either CreateWalletError Wallet)
createWallet pwl = pwl ^. pwlCreateWallet

getWallets :: forall m. PassiveWalletLayer m -> m (IxSet Wallet)
getWallets pwl = pwl ^. pwlGetWallets

getWallet :: forall m. PassiveWalletLayer m
          -> WalletId
          -> m (Either GetWalletError Wallet)
getWallet pwl = pwl ^. pwlGetWallet

updateWallet :: forall m. PassiveWalletLayer m
             -> WalletId
             -> WalletUpdate
             -> m (Either UpdateWalletError Wallet)
updateWallet pwl = pwl ^. pwlUpdateWallet

updateWalletPassword :: forall m. PassiveWalletLayer m
                     -> WalletId
                     -> PasswordUpdate
                     -> m (Either UpdateWalletPasswordError Wallet)
updateWalletPassword pwl = pwl ^. pwlUpdateWalletPassword

deleteWallet :: forall m. PassiveWalletLayer m
             -> WalletId
             -> m (Either DeleteWalletError ())
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

getAccountBalance :: forall m. PassiveWalletLayer m
                  -> WalletId
                  -> AccountIndex
                  -> m (Either GetAccountError AccountBalance)
getAccountBalance pwl = pwl ^. pwlGetAccountBalance

getAccountAddresses  :: forall m. PassiveWalletLayer m
                     -> WalletId
                     -> AccountIndex
                     -> RequestParams
                     -> FilterOperations '[V1 Address] WalletAddress
                     -> m (Either GetAccountError (WalletResponse [WalletAddress]))
getAccountAddresses pwl = pwl ^. pwlGetAccountAddresses

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

createAddress :: forall m. PassiveWalletLayer m
              -> NewAddress
              -> m (Either CreateAddressError WalletAddress)
createAddress pwl = pwl ^. pwlCreateAddress

getAddresses :: forall m. PassiveWalletLayer m
             -> RequestParams
             -> m (SliceOf WalletAddress)
getAddresses pwl = pwl ^. pwlGetAddresses

validateAddress :: forall m. PassiveWalletLayer m
                -> Text
                -> m (Either ValidateAddressError WalletAddress)
validateAddress pwl = pwl ^. pwlValidateAddress

getTransactions :: forall m. PassiveWalletLayer m -> Maybe WalletId -> Maybe AccountIndex
    -> Maybe (V1 Address) -> RequestParams -> FilterOperations '[V1 TxId, V1 Timestamp] Transaction -> SortOperations Transaction -> m (Either GetTxError (WalletResponse [Transaction]))
getTransactions pwl = pwl ^. pwlGetTransactions

getTxFromMeta :: forall m. PassiveWalletLayer m -> TxMeta -> m (Either Kernel.UnknownHdAccount Transaction)
getTxFromMeta pwl = pwl ^. pwlGetTxFromMeta

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
          -- The \"spending password\" to decrypt the 'EncryptedSecretKey'.
          -> InputGrouping
          -- An preference on how to group inputs during coin selection.
          -> ExpenseRegulation
          -- Who pays the fee, if the sender or the receivers.
          -> Payment
          -- The payment we need to perform.
          -> m (Either NewPaymentError (Tx, TxMeta))

      -- | Estimates the fees for a payment.
    , estimateFees :: PassPhrase
                   -- The \"spending password\" to decrypt the 'EncryptedSecretKey'.
                   -> InputGrouping
                   -- An preference on how to group inputs during coin selection
                   -> ExpenseRegulation
                   -- Who pays the fee, if the sender or the receivers.
                   -> Payment
                   -- The payment we need to perform.
                   -> m (Either EstimateFeesError Coin)

      -- | Redeem ada
    , redeemAda :: Redemption -> m (Either RedeemAdaError Tx)
    }

------------------------------------------------------------
-- Active wallet errors
------------------------------------------------------------

data NewPaymentError =
      NewPaymentError Kernel.PaymentError
    | NewPaymentTimeLimitReached TimeExecutionLimit
    | NewPaymentWalletIdDecodingFailed Text
    | NewPaymentUnknownAccountId Kernel.UnknownHdAccount

-- | Unsound show instance needed for the 'Exception' instance.
instance Show NewPaymentError where
    show = formatToString build

instance Exception NewPaymentError

instance Buildable NewPaymentError where
    build (NewPaymentError kernelErr) =
        bprint ("NewPaymentError " % build) kernelErr
    build (NewPaymentTimeLimitReached ter) =
        bprint ("NewPaymentTimeLimitReached " % build) ter
    build (NewPaymentWalletIdDecodingFailed txt) =
        bprint ("NewPaymentWalletIdDecodingFailed " % build) txt
    build (NewPaymentUnknownAccountId err) =
        bprint ("NewPaymentUnknownAccountId " % build) err


data EstimateFeesError =
      EstimateFeesError Kernel.EstimateFeesError
    | EstimateFeesTimeLimitReached TimeExecutionLimit
    | EstimateFeesWalletIdDecodingFailed Text

-- | Unsound show instance needed for the 'Exception' instance.
instance Show EstimateFeesError where
    show = formatToString build

instance Exception EstimateFeesError

instance Buildable EstimateFeesError where
    build (EstimateFeesError kernelErr) =
        bprint ("EstimateFeesError " % build) kernelErr
    build (EstimateFeesTimeLimitReached ter) =
        bprint ("EstimateFeesTimeLimitReached " % build) ter
    build (EstimateFeesWalletIdDecodingFailed txt) =
        bprint ("EstimateFeesWalletIdDecodingFailed " % build) txt

instance Arbitrary EstimateFeesError where
    arbitrary = oneof [ EstimateFeesError <$> arbitrary
                      , EstimateFeesTimeLimitReached <$> arbitrary
                      ]

-- | TODO: Will need to be extended
data RedeemAdaError = RedeemAdaError

instance Show RedeemAdaError where
    show = formatToString build

instance Exception RedeemAdaError

instance Buildable RedeemAdaError where
    build RedeemAdaError = "RedeemAdaError"
