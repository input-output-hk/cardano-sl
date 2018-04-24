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

import           Cardano.Wallet.API.V1.Types (AccountIndex, WalletId)


data WalletLayerError
    = WalletNotFound WalletId
    | AccountNotFound WalletId AccountIndex
    | AddressNotFound WalletId AccountIndex
    -- NOTE: optionally show mnemonic of the wallet (adn thinks its a bad idea https://iohk.myjetbrains.com/youtrack/issue/CSL-2431#comment=93-19922 )
    | WalletAlreadyExists
    deriving (Show, Eq, Generic)

instance Exception WalletLayerError

instance Buildable WalletLayerError where
    build (WalletNotFound  wId      ) = bprint ("Wallet not found. Wallet id ("%stext%").") (show wId)
    build (AccountNotFound wId accIx) = bprint ("Account not found. Wallet id ("%stext%"), accound index ("%stext%").") (show wId) (show accIx)
    build (AddressNotFound wId accIx) = bprint ("Address not found. Wallet id ("%stext%"), accound index ("%stext%").") (show wId) (show accIx)
    build WalletAlreadyExists         = "Can't create a wallet. The wallet already exists."
