-- | Types describing runtime errors related to Wallet.

module Pos.Wallet.Web.Error.Types
       ( WalletError (..)
       , _InternalError
       , _RequestError
       , _DuplicateWalletError
       , _NoSuchWalletError
       , _DecodeError
       ) where

import           Universum

import           Control.Lens (makePrisms)
import           Formatting (bprint, stext, (%))
import qualified Formatting.Buildable

data WalletError
    -- | Reasonable error for given request
    -- (e.g. get info about non-existent wallet).
    -- However, this separation is still a bit conditional, may require remake
    = RequestError !Text
    -- | Special error: duplicate wallets (both regular and internal ones) are disallowed
    | DuplicateWalletError !Text
    -- | Special error: specified wallet not found
    | NoSuchWalletError !Text
    -- | Internal info, which ideally should never happen
    | InternalError !Text
    -- | Failed to decode from @CType@ to original type
    | DecodeError !Text
    deriving (Show, Generic)

makePrisms ''WalletError

instance Exception WalletError

instance Buildable WalletError where
    build (RequestError  msg)        = bprint ("Request error ("%stext%")") msg
    build (DuplicateWalletError msg) = bprint ("Wallet with id "%stext%" already exists") msg
    build (NoSuchWalletError msg)    = bprint ("Wallet with id "%stext%" not found") msg
    build (InternalError msg)        = bprint ("Internal error ("%stext%")") msg
    build (DecodeError   msg)        = bprint ("Decoding error ("%stext%")") msg
