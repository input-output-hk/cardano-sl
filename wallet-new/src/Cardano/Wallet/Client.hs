-- | This module defines a client for the Cardano new-wallet.
module Cardano.Wallet.Client
    ( -- * The abstract client
      WalletClient(..)
    , getWalletIndex
    , Resp
    -- * The type of errors that the client might return
    , WalletError(..)
    -- * Reexports
    , module Cardano.Wallet.API.V1.Types
    , module Cardano.Wallet.API.V1.Parameters
    , module Cardano.Wallet.API.Request.Pagination
    ) where

import           Universum

import           Cardano.Wallet.API.Response

import           Cardano.Wallet.API.Request.Pagination
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
      getAddressIndex :: Resp m [Address]
    , postAddress :: NewAddress -> Resp m WalletAddress
    , getAddressValidity :: Text -> Resp m AddressValidity
    -- wallets endpoints
    , postWallet :: New Wallet -> Resp m Wallet
    , getWalletIndexPaged :: Maybe Page -> Maybe PerPage -> Resp m [Wallet]
    , updateWalletPassword :: WalletId -> PasswordUpdate -> Resp m Wallet
    , deleteWallet :: WalletId -> Resp m ()
    , getWallet :: WalletId -> Resp m Wallet
    , updateWallet :: WalletId -> Update Wallet -> Resp m Wallet
    -- account endpoints
    , deleteAccount :: WalletId -> AccountIndex -> Resp m ()
    , getAccount :: WalletId -> AccountIndex -> Resp m Account
    , getAccountIndexPaged :: WalletId -> Maybe Page -> Maybe PerPage -> Resp m [Account]
    , updateAccount :: WalletId -> AccountIndex -> Update Account -> Resp m Account
    -- transactions endpoints
    , postTransaction :: Payment -> Resp m Transaction
    , getTranasactionIndex :: WalletId -> Maybe AccountIndex -> Maybe (V1 Core.Address) -> Maybe Page -> Maybe PerPage -> Resp m [Transaction]
    , getTransactionFee :: Payment -> Resp m EstimatedFees
    -- updates
    , getNextUpdate :: Resp m WalletUpdate
    , postWalletUpdate :: Resp m WalletUpdate
    -- settings
    , getNodeSettings :: Resp m NodeSettings
    -- info
    , getNodeInfo :: Resp m NodeInfo
    }

-- | Calls 'getWalletIndexPaged' using the 'Default' values for 'Page' and
-- 'PerPage'.
getWalletIndex :: WalletClient m -> Resp m [Wallet]
getWalletIndex wc = getWalletIndexPaged wc Nothing Nothing

-- | A type alias shorthand for the response from the 'WalletClient'.
type Resp m a = m (Either WalletError (WalletResponse a))
