{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

-- | This module defines a client interface for the Cardano wallet.
module Cardano.Wallet.Client
    ( -- * The abstract client
      WalletClient(..)
    , getWalletIndex
    , Resp
    , hoistClient
    , liftClient
    -- * The type of errors that the client might return
    , ClientError(..)
    , WalletError(..)
    , ServantError(..)
    -- * Reexports
    , module Cardano.Wallet.API.V1.Types
    , module Cardano.Wallet.API.V1.Parameters
    , module Cardano.Wallet.API.Request.Pagination
    ) where

import           Universum

import           Servant.Client (ServantError (..))

import           Cardano.Wallet.API.Request.Pagination
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types
import qualified Pos.Core as Core

-- | A representation of a wallet client parameterized over some effect
-- type @m@.
--
-- The record fields serve as the API to the wallet client. Note that the
-- field 'getAddressIndex' has the type:
--
-- @
-- 'getAddressIndex'
--     :: 'WalletClient' m
--     -> m ('Either' 'WalletError' ('WalletResponse' ['Address']))
-- @
--
-- Other functions may be defined in terms of this 'WalletClient' -- see
-- 'getWalletIndex' as a convenience helper for 'getWalletIndexPaged'.
data WalletClient m
    = WalletClient
    { -- address endpoints
      getAddressIndex
         :: Resp m [Address]
    , postAddress
         :: NewAddress -> Resp m WalletAddress
    , getAddressValidity
         :: Text -> Resp m AddressValidity
    -- wallets endpoints
    , postWallet
         :: New Wallet -> Resp m Wallet
    , getWalletIndexPaged
         :: Maybe Page -> Maybe PerPage -> Resp m [Wallet]
    , updateWalletPassword
         :: WalletId -> PasswordUpdate -> Resp m Wallet
    , deleteWallet
         :: WalletId -> Resp m ()
    , getWallet
         :: WalletId -> Resp m Wallet
    , updateWallet
         :: WalletId -> Update Wallet -> Resp m Wallet
    -- account endpoints
    , deleteAccount
         :: WalletId -> AccountIndex -> Resp m ()
    , getAccount
         :: WalletId -> AccountIndex -> Resp m Account
    , getAccountIndexPaged
         :: WalletId -> Maybe Page -> Maybe PerPage -> Resp m [Account]
    , updateAccount
         :: WalletId -> AccountIndex -> Update Account -> Resp m Account
    -- transactions endpoints
    , postTransaction
         :: Payment -> Resp m Transaction
    , getTransactionIndex
         :: WalletId
         -> Maybe AccountIndex
         -> Maybe (V1 Core.Address)
         -> Maybe Page
         -> Maybe PerPage
         -> Resp m [Transaction]
    , getTransactionFee
         :: Payment -> Resp m EstimatedFees
    -- updates
    , getNextUpdate
         :: Resp m WalletUpdate
    , postWalletUpdate
         :: Resp m WalletUpdate
    -- settings
    , getNodeSettings
         :: Resp m NodeSettings
    -- info
    , getNodeInfo
         :: Resp m NodeInfo
    }

-- | Run the given natural transformation over the 'WalletClient'.
hoistClient :: (forall x. m x -> n x) -> WalletClient m -> WalletClient n
hoistClient phi wc = WalletClient
    { getAddressIndex       = phi (getAddressIndex wc)
    , postAddress           = phi . postAddress wc
    , getAddressValidity    = phi . getAddressValidity wc
    , postWallet            = phi . postWallet wc
    , getWalletIndexPaged   = \x -> phi . getWalletIndexPaged wc x
    , updateWalletPassword  = \x -> phi . updateWalletPassword wc x
    , deleteWallet          = phi . deleteWallet wc
    , getWallet             = phi . getWallet wc
    , updateWallet          = \x -> phi . updateWallet wc x
    , deleteAccount         = \x -> phi . deleteAccount wc x
    , getAccount            = \x -> phi . getAccount wc x
    , getAccountIndexPaged  = \x mp -> phi . getAccountIndexPaged wc x mp
    , updateAccount         = \x y -> phi . updateAccount wc x y
    , postTransaction       = phi . postTransaction wc
    , getTransactionIndex   = \wid maid maddr mp -> phi . getTransactionIndex wc wid maid maddr mp
    , getTransactionFee     = phi . getTransactionFee wc
    , getNextUpdate         = phi (getNextUpdate wc)
    , postWalletUpdate      = phi (postWalletUpdate wc)
    , getNodeSettings       = phi (getNodeSettings wc)
    , getNodeInfo           = phi (getNodeInfo wc)
    }

-- | Generalize a @'WalletClient' 'IO'@ into a @('MonadIO' m) =>
-- 'WalletClient' m@.
liftClient :: MonadIO m => WalletClient IO -> WalletClient m
liftClient = hoistClient liftIO

-- | Calls 'getWalletIndexPaged' using the 'Default' values for 'Page' and
-- 'PerPage'.
getWalletIndex :: WalletClient m -> Resp m [Wallet]
getWalletIndex wc = getWalletIndexPaged wc Nothing Nothing

-- | A type alias shorthand for the response from the 'WalletClient'.
type Resp m a = m (Either ClientError (WalletResponse a))

-- | The type of errors that the wallet might return.
data ClientError
    = ClientWalletError WalletError
    -- ^ The 'WalletError' type represents known failures that the API
    -- might return.
    | ClientHttpError ServantError
    -- ^ We directly expose the 'ServantError' type as part of this
    | UnknownError SomeException
    -- ^ This constructor is used when the API client reports an error that
    -- isn't represented in either the 'ServantError' HTTP errors or the
    -- 'WalletError' for API errors.
