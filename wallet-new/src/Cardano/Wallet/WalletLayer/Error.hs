{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

-- | Types describing runtime errors related to
-- wallet layers. It should be a common interface for
-- all the errors popping up from the @WalletLayer@.

module Cardano.Wallet.WalletLayer.Error
    ( CreateWalletException (..)
    , GetWalletIdsException (..)
    , GetWalletException (..)
    , UpdateWalletException (..)
    , DeleteWalletException (..)
    -- accounts
    , CreateAccountException (..)
    , GetAccountsException (..)
    , GetAccountException (..)
    , UpdateAccountException (..)
    , DeleteAccountException (..)
    -- addresses
    , CreateAddressException (..)
    , GetAddressesException (..)
    , IsValidAddressException (..)
    -- transactions
    , CreateTxException (..)
    , GetTxException (..)
    , FeeEstimateException (..)
    -- settings
    , GetSettingsException (..)
    -- info
    , GetInfoException (..)
    ) where

import           Universum

import           Data.Text.Buildable (build)
import           Formatting (bprint, stext, (%))

import           Cardano.Wallet.API.V1.Types (AccountIndex, WalletAddress, WalletId)

------------------------------------------------------------
-- General wallet error handlers
------------------------------------------------------------

-- | Some of the handlers could have @UndefinedException@ as the
-- error/exception type, which simply means that we reached the base
-- of the exception stack and we don't know what else it could be.
data UndefinedException
    = UndefinedException
    deriving (Show, Eq, Generic)

instance Exception UndefinedException

instance Buildable UndefinedException where
    build = \case
        UndefinedException                  -> bprint ("An unexpected exception occured. Please report this!")


-- | These are the general exceptions that can be returned
-- from the code. They are general since they can be found
-- in several endpoints, so we can minimize the duplication.
data GeneralException
    = WalletNotFound WalletId
    | AccountNotFound AccountIndex
    | AddressNotFound WalletAddress
    deriving (Show, Eq, Generic)

instance Exception GeneralException

instance Buildable GeneralException where
    build = \case
        WalletNotFound wId                  -> bprint ("Wallet not found. Wallet id ("%stext%").") (show wId)
        AccountNotFound aId                 -> bprint ("Account not found. Account id ("%stext%").") (show aId)
        AddressNotFound wAddr               -> bprint ("Wallet address not found. Address is ("%stext%").") (show wAddr)

-- TODO(ks): We could also use something similar as
-- a base of our exceptions. That way, we could have base exceptions
-- everywhere. For example, `DatabaseFailure`.
--
-- Also, we can see the repetition below, but we might want to wait
-- a while until the interface stabilizes when we build the new @Kernel@, and
-- then generalize it if required.

------------------------------------------------------------
-- Specific wallet error handlers
------------------------------------------------------------

data CreateWalletException
    = CreateWalletWalletNotFound WalletId
    | CreateWalletNodeSyncing
    deriving (Show, Eq, Generic)

instance Exception CreateWalletException

instance Buildable CreateWalletException where
    build = \case
        CreateWalletWalletNotFound wId      -> build $ WalletNotFound wId
        CreateWalletNodeSyncing             -> bprint ("Node is still syncing, cannot create new wallet.")


-- | For now, we really don't have much to go on.
data GetWalletIdsException
    = GetWalletIdsUndefined UndefinedException
    deriving (Show, Eq, Generic)

instance Exception GetWalletIdsException

instance Buildable GetWalletIdsException where
    build = \case
        GetWalletIdsUndefined ue            -> build ue


data GetWalletException
    = GetWalletWalletNotFound WalletId
    deriving (Show, Eq, Generic)

instance Exception GetWalletException

instance Buildable GetWalletException where
    build = \case
        GetWalletWalletNotFound wId         -> build $ WalletNotFound wId


data UpdateWalletException
    = UpdateWalletWalletNotFound WalletId
    deriving (Show, Eq, Generic)

instance Exception UpdateWalletException

instance Buildable UpdateWalletException where
    build = \case
        UpdateWalletWalletNotFound wId      -> build $ WalletNotFound wId


data DeleteWalletException
    = DeleteWalletWalletNotFound WalletId
    deriving (Show, Eq, Generic)

instance Exception DeleteWalletException

instance Buildable DeleteWalletException where
    build = \case
        DeleteWalletWalletNotFound wId      -> build $ WalletNotFound wId

------------------------------------------------------------
-- Specific account error handlers
------------------------------------------------------------

data CreateAccountException
    = CreateAccountWalletNotFound WalletId
    | CreateAccountAccountNotFound AccountIndex
    deriving (Show, Eq, Generic)

instance Exception CreateAccountException

instance Buildable CreateAccountException where
    build = \case
        CreateAccountWalletNotFound wId     -> build $ WalletNotFound wId
        CreateAccountAccountNotFound aId    -> build $ AccountNotFound aId


data GetAccountsException
    = GetAccountsWalletNotFound WalletId
    deriving (Show, Eq, Generic)

instance Exception GetAccountsException

instance Buildable GetAccountsException where
    build = \case
        GetAccountsWalletNotFound wId       -> build $ WalletNotFound wId


data GetAccountException
    = GetAccountWalletNotFound WalletId
    | GetAccountAccountNotFound AccountIndex
    deriving (Show, Eq, Generic)

instance Exception GetAccountException

instance Buildable GetAccountException where
    build = \case
        GetAccountWalletNotFound wId        -> build $ WalletNotFound wId
        GetAccountAccountNotFound aId       -> build $ AccountNotFound aId


data UpdateAccountException
    = UpdateAccountWalletNotFound WalletId
    | UpdateAccountAccountNotFound AccountIndex
    deriving (Show, Eq, Generic)

instance Exception UpdateAccountException

instance Buildable UpdateAccountException where
    build = \case
        UpdateAccountWalletNotFound wId     -> build $ WalletNotFound wId
        UpdateAccountAccountNotFound aId    -> build $ AccountNotFound aId


data DeleteAccountException
    = DeleteAccountWalletNotFound WalletId
    | DeleteAccountAccountNotFound AccountIndex
    deriving (Show, Eq, Generic)

instance Exception DeleteAccountException

instance Buildable DeleteAccountException where
    build = \case
        DeleteAccountWalletNotFound wId     -> build $ WalletNotFound wId
        DeleteAccountAccountNotFound aId    -> build $ AccountNotFound aId

------------------------------------------------------------
-- Specific address error handlers
------------------------------------------------------------

data CreateAddressException
    = CreateAddressWalletNotFound WalletId
    | CreateAddressAccountNotFound AccountIndex
    deriving (Show, Eq, Generic)

instance Exception CreateAddressException

instance Buildable CreateAddressException where
    build = \case
        CreateAddressWalletNotFound wId     -> build $ WalletNotFound wId
        CreateAddressAccountNotFound aId    -> build $ AccountNotFound aId


data GetAddressesException
    = GetAddressesWalletNotFound WalletId
    | GetAddressesAccountNotFound AccountIndex
    deriving (Show, Eq, Generic)

instance Exception GetAddressesException

instance Buildable GetAddressesException where
    build = \case
        GetAddressesWalletNotFound wId      -> build $ WalletNotFound wId
        GetAddressesAccountNotFound aId     -> build $ AccountNotFound aId


data IsValidAddressException
    = IsValidAddrWalletAddrNotFound WalletAddress
    deriving (Show, Eq, Generic)

instance Exception IsValidAddressException

instance Buildable IsValidAddressException where
    build = \case
        IsValidAddrWalletAddrNotFound wAddr -> build $ AddressNotFound wAddr


------------------------------------------------------------
-- Specific transaction error handlers
------------------------------------------------------------

data CreateTxException
    = CreateTxWalletNotFound WalletId
    | CreateTxAccountNotFound AccountIndex
    deriving (Show, Eq, Generic)

instance Exception CreateTxException

instance Buildable CreateTxException where
    build = \case
        CreateTxWalletNotFound wId          -> build $ WalletNotFound wId
        CreateTxAccountNotFound aId         -> build $ AccountNotFound aId


data GetTxException
    = GetTxWalletNotFound WalletId
    | GetTxAccountNotFound AccountIndex
    | GetTxAddressNotFound WalletAddress
    deriving (Show, Eq, Generic)

instance Exception GetTxException

instance Buildable GetTxException where
    build = \case
        GetTxWalletNotFound wId             -> build $ WalletNotFound wId
        GetTxAccountNotFound aId            -> build $ AccountNotFound aId
        GetTxAddressNotFound wAddr          -> build $ AddressNotFound wAddr


data FeeEstimateException
    = FeeEstimateWalletNotFound WalletId
    | FeeEstimateAccountNotFound AccountIndex
    | FeeEstimateAddressNotFound WalletAddress
    deriving (Show, Eq, Generic)

instance Exception FeeEstimateException

instance Buildable FeeEstimateException where
    build = \case
        FeeEstimateWalletNotFound wId       -> build $ WalletNotFound wId
        FeeEstimateAccountNotFound aId      -> build $ AccountNotFound aId
        FeeEstimateAddressNotFound wAddr    -> build $ AddressNotFound wAddr

------------------------------------------------------------
-- Specific settings error handlers
------------------------------------------------------------

data GetSettingsException
    = GetSettingsUndefined UndefinedException
    deriving (Show, Eq, Generic)

instance Exception GetSettingsException

instance Buildable GetSettingsException where
    build = \case
        GetSettingsUndefined ue             -> build ue

------------------------------------------------------------
-- Specific info error handlers
------------------------------------------------------------

data GetInfoException
    = GetInfoUndefined UndefinedException
    deriving (Show, Eq, Generic)

instance Exception GetInfoException

instance Buildable GetInfoException where
    build = \case
        GetInfoUndefined ue                 -> build ue


