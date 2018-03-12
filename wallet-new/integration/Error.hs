{-# LANGUAGE DeriveGeneric #-}

-- | Types describing runtime errors related to
-- wallet integration tests.

module Error
    ( WalletTestError (..)
    ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))

import           Cardano.Wallet.API.V1.Types (Account (..), Wallet (..))

import           Cardano.Wallet.Client (ClientError)


data WalletTestError
    = HttpClientError ClientError

    | WalletBalanceNotZero Wallet
    | LocalWalletDiffers Wallet
    | LocalWalletsDiffers [Wallet]

    | AccountBalanceNotZero Account
    | LocalAccountDiffers Account
    | LocalAccountsDiffers [Account]

    deriving (Show, Eq, Generic)


instance Exception WalletTestError


instance Buildable WalletTestError where
    build (HttpClientError _      )  = bprint "Http client error"
    -- ^ TODO (ks): A proper instance
    build (WalletBalanceNotZero  w)  = bprint ("Wallet balance is not zero. Wallet - ("%stext%")") (show w)
    build (LocalWalletDiffers    w)  = bprint ("Local wallet differs. Wallet - ("%stext%")") (show w)
    build (LocalWalletsDiffers   w)  = bprint ("Local wallets differs. Wallet - ("%stext%")") (show w)

    build (AccountBalanceNotZero a)  = bprint ("Acccount balance is not zero. Account - ("%stext%")") (show a)
    build (LocalAccountDiffers   a)  = bprint ("Local account differs. Account - ("%stext%")") (show a)
    build (LocalAccountsDiffers  a)  = bprint ("Local accounts differs. Account - ("%stext%")") (show a)


