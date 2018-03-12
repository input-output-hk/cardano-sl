{-# LANGUAGE DeriveGeneric #-}

-- | Types describing runtime errors related to
-- wallet integration tests.

module Error
    ( WalletTestError (..)
    ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))

import           Cardano.Wallet.Client (ClientError)

data WalletTestError
    = Internal Text
    | HttpClientError ClientError
    | ServerConnectionFailed
    deriving (Show, Eq, Generic)


instance Exception WalletTestError


instance Buildable WalletTestError where
    build (Internal msg)           = bprint ("Wallet test error ("%stext%")") msg
    build (ServerConnectionFailed) = bprint "Server connection failed"
    -- TODO (ks): A proper instance
    build (HttpClientError _     ) = bprint "Http client error"
