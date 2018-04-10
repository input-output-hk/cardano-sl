{-# LANGUAGE DeriveGeneric #-}

-- | Types describing runtime errors related to
-- wallet layers. It should be a common interface for
-- all the errors popping up from the @WalletLayer@.

module Cardano.Wallet.WalletLayer.Error
    ( WalletLayerError (..)
    ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))

import           Cardano.Wallet.API.V1.Types (WalletId)


data WalletLayerError
    = MissingWallet WalletId
    | MissingAccount
    deriving (Show, Eq, Generic)

instance Exception WalletLayerError

instance Buildable WalletLayerError where
    build (MissingWallet wId)   = bprint ("Missing wallet with wallet id ("%stext%").") (show wId)
    build (MissingAccount)      = bprint ("Missing account.")

