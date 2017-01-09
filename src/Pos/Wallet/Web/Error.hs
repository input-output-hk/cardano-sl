-- | Types describing runtime errors related to Wallet.

module Pos.Wallet.Web.Error
       ( WalletError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))
import           Universum

data WalletError =
    -- | Some internal error.
    Internal !Text
    deriving (Show, Generic)

instance Exception WalletError

instance Buildable WalletError where
    build (Internal msg) = bprint ("Internal wallet error ("%stext%")") msg
