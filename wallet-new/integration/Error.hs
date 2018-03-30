{-# LANGUAGE DeriveGeneric #-}

-- | Types describing runtime errors related to
-- wallet integration tests.

module Error
    ( WalletTestError (..)
    ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))

import           Cardano.Wallet.API.V1.Types (Account, Address, EstimatedFees, Transaction, Wallet,
                                              WalletAddress)

import           Cardano.Wallet.Client (ClientError)


data WalletTestError
    = HttpClientError ClientError

    | InvalidProbabilityDistr

    | WalletBalanceNotZero Wallet
    | WalletPassMissing Wallet
    | LocalWalletDiffers Wallet
    | LocalWalletsDiffers [Wallet]

    | AccountBalanceNotZero Account
    | LocalAccountDiffers Account
    | LocalAccountsDiffers [Account]

    | AddressBalanceNotZero WalletAddress
    | LocalAddressesDiffer WalletAddress [WalletAddress]
    | LocalAddressDiffer Address

    | InvalidTransactionState Transaction
    | InvalidTransactionFee EstimatedFees
    | LocalTransactionsDiffer [Transaction]
    | LocalTransactionMissing Transaction [Transaction]

    deriving (Show, Eq, Generic)


instance Exception WalletTestError


instance Buildable WalletTestError where
    build (HttpClientError _        )     = bprint "Http client error"
    -- ^ TODO (ks): A proper instance
    build InvalidProbabilityDistr         = bprint "The probability distribution should be between 1 - 100."
    build (WalletBalanceNotZero    w)     = bprint ("Wallet balance is not zero - ("%stext%")") (show w)
    build (WalletPassMissing       w)     = bprint ("Missing wallet pass - ("%stext%")") (show w)
    build (LocalWalletDiffers      w)     = bprint ("Local wallet differs - ("%stext%")") (show w)
    build (LocalWalletsDiffers     w)     = bprint ("Local wallets differs - ("%stext%")") (show w)

    build (AccountBalanceNotZero   a)     = bprint ("Acccount balance is not zero - ("%stext%")") (show a)
    build (LocalAccountDiffers     a)     = bprint ("Local account differs - ("%stext%")") (show a)
    build (LocalAccountsDiffers    a)     = bprint ("Local accounts differs - ("%stext%")") (show a)

    build (AddressBalanceNotZero   a)     = bprint ("Address balance is not zero - ("%stext%")") (show a)
    build (LocalAddressesDiffer a as)     = bprint ("Local address ("%stext%") missing from addresses ("%stext%")") (show a) (show as)
    build (LocalAddressDiffer      a)     = bprint ("Local address differs - ("%stext%")") (show a)

    build (InvalidTransactionState t)     = bprint ("Transaction state is invalid. Transaction - ("%stext%")") (show t)
    build (InvalidTransactionFee   f)     = bprint ("Transaction fees are invalid - ("%stext%")") (show f)
    build (LocalTransactionsDiffer t)     = bprint ("Local transactions differs - ("%stext%")") (show t)
    build (LocalTransactionMissing t ts)  = bprint ("Local transaction ("%stext%") missing from txs history ("%stext%")") (show t) (show ts)

