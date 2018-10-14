module Cardano.Wallet.WalletLayer
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    -- * Types
    , CreateWallet(..)
    -- ** Errors
    , CreateWalletError(..)
    , GetWalletError(..)
    , UpdateWalletError(..)
    , UpdateWalletPasswordError(..)
    , DeleteWalletError(..)
    , GetUtxosError(..)
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
    , ImportWalletError(..)
    , NewUnsignedTransactionError(..)
    , SubmitSignedTransactionError(..)
    ) where

import           Universum

import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import qualified Prelude
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Pos.Chain.Block (Blund)
import           Pos.Chain.Txp (Tx, TxId, Utxo)
import           Pos.Chain.Update (ConfirmedProposalState, SoftwareVersion)
import           Pos.Core (Coin, Timestamp)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Crypto (EncryptedSecretKey, PassPhrase)

import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
import           Cardano.Wallet.API.Request.Sort (SortOperations (..))
import           Cardano.Wallet.API.Response (SliceOf (..), WalletResponse)
import           Cardano.Wallet.API.V1.Types (Account, AccountBalance,
                     AccountIndex, AccountUpdate, Address,
                     Base58PublicKeyError (..), ForceNtpCheck, NewAccount,
                     NewAddress, NewExternalWallet, NewWallet, NodeInfo,
                     NodeSettings, PasswordUpdate, Payment, PublicKeyAsBase58,
                     Redemption, SignedTransaction, SpendingPassword,
                     Transaction, UnsignedTransaction, V1 (..), Wallet,
                     WalletAddress, WalletId, WalletImport, WalletUpdate)
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
import           Cardano.Wallet.WalletLayer.Kernel.Conv (InvalidRedemptionCode)

------------------------------------------------------------
-- Type & Errors when manipulating wallets
------------------------------------------------------------

data CreateWallet =
    CreateWallet NewWallet
  | CreateExternalWallet NewExternalWallet
  | ImportWalletFromESK EncryptedSecretKey (Maybe SpendingPassword)

data CreateWalletError =
    CreateWalletError Kernel.CreateWalletError
  | CreateWalletInvalidRootPK Base58PublicKeyError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateWalletError where
    show = formatToString build

instance Exception CreateWalletError

instance Arbitrary CreateWalletError where
    arbitrary = oneof [ CreateWalletError <$> arbitrary
                      , pure (CreateWalletInvalidRootPK PublicKeyNotInBase58Form)
                      ]

instance Buildable CreateWalletError where
    build (CreateWalletError kernelError) =
        bprint ("CreateWalletError " % build) kernelError
    build (CreateWalletInvalidRootPK pkError) =
        bprint ("CreateWalletInvalidRootPK " % build) pkError

data GetWalletError =
      GetWalletError Kernel.UnknownHdRoot
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
    build (GetWalletError kernelError) =
        bprint ("GetWalletError " % build) kernelError
    build (GetWalletErrorNotFound walletId) =
        bprint ("GetWalletErrorNotFound " % build) walletId
    build (GetWalletWalletIdDecodingFailed txt) =
        bprint ("GetWalletWalletIdDecodingFailed " % build) txt

data UpdateWalletError =
      UpdateWalletError Kernel.UnknownHdRoot
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
    build (UpdateWalletError kernelError) =
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
    | DeleteWalletError Kernel.UnknownHdRoot
    | DeleteWalletInvalidRootPK Base58PublicKeyError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show DeleteWalletError where
    show = formatToString build

instance Exception DeleteWalletError

instance Buildable DeleteWalletError where
    build (DeleteWalletWalletIdDecodingFailed txt) =
        bprint ("DeleteWalletWalletIdDecodingFailed " % build) txt
    build (DeleteWalletError kernelError) =
        bprint ("DeleteWalletError " % build) kernelError
    build (DeleteWalletInvalidRootPK pkError) =
        bprint ("DeleteWalletInvalidRootPK " % build) pkError

data GetUtxosError =
      GetUtxosWalletIdDecodingFailed Text
    | GetUtxosGetAccountsError Kernel.UnknownHdRoot
    | GetUtxosCurrentAvailableUtxoError Kernel.UnknownHdAccount
    deriving Eq

instance Show GetUtxosError where
    show = formatToString build

instance Exception GetUtxosError

instance Buildable GetUtxosError where
    build (GetUtxosWalletIdDecodingFailed txt) =
        bprint ("GetUtxosWalletIdDecodingFailed " % build) txt
    build (GetUtxosGetAccountsError kernelError) =
        bprint ("GetUtxosGetAccountsError " % build) kernelError
    build (GetUtxosCurrentAvailableUtxoError kernelError) =
        bprint ("GetUtxosCurrentAvailableUtxoError " % build) kernelError

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
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show ValidateAddressError where
    show = formatToString build

instance Exception ValidateAddressError

instance Buildable ValidateAddressError where
    build (ValidateAddressDecodingFailed rawText) =
        bprint ("ValidateAddressDecodingFailed " % build) rawText

------------------------------------------------------------
-- Errors when dealing with Accounts
------------------------------------------------------------

data CreateAccountError =
      CreateAccountError Kernel.CreateAccountError
    | CreateAccountWalletIdDecodingFailed Text
    -- ^ Decoding the parent's 'WalletId' from a raw 'Text' failed.
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateAccountError where
    show = formatToString build

instance Exception CreateAccountError

instance Arbitrary CreateAccountError where
    arbitrary = oneof [ CreateAccountError <$> arbitrary
                      , CreateAccountWalletIdDecodingFailed <$> arbitrary
                      ]

instance Buildable CreateAccountError where
    build (CreateAccountError kernelError) =
        bprint ("CreateAccountError " % build) kernelError
    build (CreateAccountWalletIdDecodingFailed txt) =
        bprint ("CreateAccountWalletIdDecodingFailed " % build) txt

data GetAccountError =
      GetAccountError Kernel.UnknownHdAccount
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
      DeleteAccountError Kernel.UnknownHdAccount
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
      UpdateAccountError Kernel.UnknownHdAccount
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

data ImportWalletError =
      ImportWalletFileNotFound FilePath
    | ImportWalletNoWalletFoundInBackup FilePath
    -- ^ When trying to fetch the required information, the legacy keystore
    -- didn't provide any.
    | ImportWalletCreationFailed CreateWalletError
    -- ^ When trying to import this wallet, the wallet creation failed.

-- | Unsound show instance needed for the 'Exception' instance.
instance Show ImportWalletError where
    show = formatToString build

instance Exception ImportWalletError

instance Buildable ImportWalletError where
    build (ImportWalletFileNotFound fp) =
        bprint ("ImportWalletFileNotFound " % build) fp
    build (ImportWalletNoWalletFoundInBackup fp) =
        bprint ("ImportWalletNoWalletFoundInBackup " % build) fp
    build (ImportWalletCreationFailed err) =
        bprint ("ImportWalletCreationFailed " % build) err

------------------------------------------------------------
-- Errors when getting Transactions
------------------------------------------------------------

data GetTxError =
      GetTxMissingWalletIdError
    | GetTxAddressDecodingFailed Text
    | GetTxInvalidSortingOperation String
    | GetTxUnknownHdAccount Kernel.UnknownHdAccount

instance Show GetTxError where
    show = formatToString build

instance Buildable GetTxError where
    build GetTxMissingWalletIdError =
        bprint "GetTxMissingWalletIdError "
    build (GetTxAddressDecodingFailed txt) =
        bprint ("GetTxAddressDecodingFailed " % build) txt
    build (GetTxInvalidSortingOperation txt) =
        bprint ("GetTxInvalidSortingOperation " % build) txt
    build (GetTxUnknownHdAccount err) =
        bprint ("GetTxUnknownHdAccount " % build) err


instance Arbitrary GetTxError where
    arbitrary = oneof [ pure GetTxMissingWalletIdError
                      , pure (GetTxAddressDecodingFailed "by_amount")
                      , pure (GetTxInvalidSortingOperation "123")
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
      createWallet         :: CreateWallet -> m (Either CreateWalletError Wallet)
    , getWallets           :: m (IxSet Wallet)
    , getWallet            :: WalletId -> m (Either GetWalletError Wallet)
    , updateWallet         :: WalletId
                           -> WalletUpdate
                           -> m (Either UpdateWalletError Wallet)
    , updateWalletPassword :: WalletId
                           -> PasswordUpdate
                           -> m (Either UpdateWalletPasswordError Wallet)
    , deleteWallet         :: WalletId -> m (Either DeleteWalletError ())
    , deleteExternalWallet :: PublicKeyAsBase58 -> m (Either DeleteWalletError ())
    , getUtxos             :: WalletId
                           -> m (Either GetUtxosError [(Account, Utxo)])
    -- accounts
    , createAccount        :: WalletId
                           -> NewAccount
                           -> m (Either CreateAccountError Account)
    , getAccounts          :: WalletId
                           -> m (Either GetAccountsError (IxSet Account))
    , getAccount           :: WalletId
                           -> AccountIndex
                           -> m (Either GetAccountError Account)
    , getAccountBalance    :: WalletId
                           -> AccountIndex
                           -> m (Either GetAccountError AccountBalance)
    , getAccountAddresses  :: WalletId
                           -> AccountIndex
                           -> RequestParams
                           -> FilterOperations '[V1 Address] WalletAddress
                           -> m (Either GetAccountError (WalletResponse [WalletAddress]))
    , updateAccount        :: WalletId
                           -> AccountIndex
                           -> AccountUpdate
                           -> m (Either UpdateAccountError Account)
    , deleteAccount        :: WalletId
                           -> AccountIndex
                           -> m (Either DeleteAccountError ())
    -- addresses
    , createAddress        :: NewAddress
                           -> m (Either CreateAddressError WalletAddress)
    , getAddresses         :: RequestParams -> m (SliceOf WalletAddress)
    , validateAddress      :: Text
                           -> m (Either ValidateAddressError WalletAddress)

    -- transactions
    , getTransactions      :: Maybe WalletId
                           -> Maybe AccountIndex
                           -> Maybe (V1 Address)
                           -> RequestParams
                           -> FilterOperations '[V1 TxId, V1 Timestamp] Transaction
                           -> SortOperations Transaction
                           -> m (Either GetTxError (WalletResponse [Transaction]))
    , getTxFromMeta        :: TxMeta -> m (Either Kernel.UnknownHdAccount Transaction)

    -- core API
    , applyBlocks          :: OldestFirst NE Blund -> m ()
    , rollbackBlocks       :: NewestFirst NE Blund -> m ()

    -- node settings
    , getNodeSettings      :: m NodeSettings

    -- internal
    , nextUpdate           :: m (Maybe (V1 SoftwareVersion))
    , applyUpdate          :: m ()
    , postponeUpdate       :: m ()
    , resetWalletState     :: m ()
    , importWallet         :: WalletImport -> m (Either ImportWalletError Wallet)

    -- updates
    , waitForUpdate        :: m ConfirmedProposalState
    , addUpdate            :: SoftwareVersion -> m ()
    }

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
    , estimateFees :: InputGrouping
                   -- An preference on how to group inputs during coin selection
                   -> ExpenseRegulation
                   -- Who pays the fee, if the sender or the receivers.
                   -> Payment
                   -- The payment we need to perform.
                   -> m (Either EstimateFeesError Coin)

      -- | Prepares unsigned transaction. Please note that this function does /not/
      -- perform a payment, it just creates a new transaction which will be signed
      -- and submitted to the blockchain later.
      --
      -- It returns transaction and list of the source addresses with corresponding
      -- derivation paths. These addresses and paths will be used by third party to
      -- provide a proof that it has a right to spend money from these addresses.
    , createUnsignedTx :: InputGrouping
                       -- An preference on how to group inputs during coin selection
                       -> ExpenseRegulation
                       -- Who pays the fee, if the sender or the receivers.
                       -> Payment
                       -- The payment we need to perform.
                       -> m (Either NewUnsignedTransactionError UnsignedTransaction)

      -- | Takes externally-signed transaction and submits it to the blockchain.
      -- The result of 'submitSignedTx' is equal to 'pay'.
    , submitSignedTx :: SignedTransaction
                     -> m (Either SubmitSignedTransactionError (Tx, TxMeta))

      -- | Redeem ada
    , redeemAda :: Redemption -> m (Either RedeemAdaError (Tx, TxMeta))

      -- | Node info
      --
      -- This lives in the active wallet layer as the node info endpoint returns
      -- status information about the diffusion layer
    , getNodeInfo :: ForceNtpCheck -> m NodeInfo
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

data RedeemAdaError =
    RedeemAdaError Kernel.RedeemAdaError
  | RedeemAdaWalletIdDecodingFailed Text
  | RedeemAdaInvalidRedemptionCode InvalidRedemptionCode

instance Show RedeemAdaError where
    show = formatToString build

instance Exception RedeemAdaError

instance Buildable RedeemAdaError where
    build (RedeemAdaError err) =
        bprint ("RedeemAdaError " % build) err
    build (RedeemAdaWalletIdDecodingFailed txt) =
        bprint ("RedeemAdaWalletIdDecodingFailed " % build) txt
    build (RedeemAdaInvalidRedemptionCode txt) =
        bprint ("RedeemAdaInvalidRedemptionCode " % build) txt

data NewUnsignedTransactionError =
      NewUnsignedTransactionError Kernel.NewTransactionError
    | NewTransactionWalletIdDecodingFailed Text

instance Show NewUnsignedTransactionError where
    show = formatToString build

instance Exception NewUnsignedTransactionError

instance Buildable NewUnsignedTransactionError where
    build (NewUnsignedTransactionError err) =
        bprint ("NewUnsignedTransactionError " % build) err
    build (NewTransactionWalletIdDecodingFailed txt) =
        bprint ("NewTransactionWalletIdDecodingFailed " % build) txt

data SubmitSignedTransactionError =
      SubmitSignedTransactionError Kernel.PaymentError
    | SubmitSignedTransactionWalletIdDecodingFailed Text
    | SubmitSignedTransactionNotBase16Format
    | SubmitSignedTransactionUnableToDecode
    | SubmitSignedTransactionInvalidSrcAddress
    | SubmitSignedTransactionSigNotBase16Format
    | SubmitSignedTransactionInvalidSig
    | SubmitSignedTransactionInvalidPK

instance Show SubmitSignedTransactionError where
    show = formatToString build

instance Exception SubmitSignedTransactionError

instance Buildable SubmitSignedTransactionError where
    build (SubmitSignedTransactionError err) =
        bprint ("NewUnsignedTransactionError " % build) err
    build (SubmitSignedTransactionWalletIdDecodingFailed txt) =
        bprint ("NewTransactionWalletIdDecodingFailed " % build) txt
    build SubmitSignedTransactionNotBase16Format =
        bprint ("SubmitSignedTransactionNotBase16Format")
    build SubmitSignedTransactionUnableToDecode =
        bprint ("SubmitSignedTransactionUnableToDecode")
    build SubmitSignedTransactionInvalidSrcAddress =
        bprint ("SubmitSignedTransactionInvalidSrcAddress")
    build SubmitSignedTransactionSigNotBase16Format =
        bprint ("SubmitSignedTransactionSigNotBase16Format")
    build SubmitSignedTransactionInvalidSig =
        bprint ("SubmitSignedTransactionInvalidSig")
    build SubmitSignedTransactionInvalidPK =
        bprint ("SubmitSignedTransactionInvalidPK")
