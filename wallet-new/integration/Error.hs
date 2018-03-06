{-# LANGUAGE DeriveGeneric #-}

-- | Types describing runtime errors related to
-- wallet integration tests.

module Error
    ( WalletTestError (..)
    ) where

import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))
import           Universum


data WalletTestError
    = Internal Text
    | ServerConnectionFailed
    deriving (Show, Eq, Generic)


instance Exception WalletTestError


instance Buildable WalletTestError where
    build (Internal msg) = bprint ("Wallet test error ("%stext%")") msg
    build (ServerConnectionFailed) = bprint "Server connection failed"
