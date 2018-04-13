{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

-- | Types describing runtime errors related to
-- wallet integration tests.
module Error
    ( WalletTestError (..)
    , showConstr
    ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))

import           Cardano.Wallet.API.V1.Types (Account, Address, EstimatedFees, Transaction, Wallet,
                                              WalletAddress)

import           Cardano.Wallet.Client (ClientError)


data WalletTestError
    = HttpClientError ClientError

    | WalletBalanceNotZero Wallet
    | WalletPassMissing Wallet
    | LocalWalletDiffers Wallet Wallet
    | LocalWalletsDiffers [Wallet] [Wallet]

    | AccountBalanceNotZero Account
    | LocalAccountDiffers Account Account
    | LocalAccountsDiffers [Account] [Account]

    | AddressBalanceNotZero WalletAddress
    | LocalAddressesDiffer [WalletAddress] [WalletAddress]
    | LocalAddressDiffer Address Address

    | InvalidTransactionState Transaction
    | InvalidTransactionFee EstimatedFees
    | LocalTransactionsDiffer [Transaction] [Transaction]
    | LocalTransactionMissing Transaction [Transaction]

    deriving (Show, Eq, Generic)

showConstr :: WalletTestError -> String
showConstr = \case
    HttpClientError {} -> "HttpClientError"
    WalletBalanceNotZero {} -> "WalletBalanceNotZero"
    WalletPassMissing {} -> "WalletPassMissing"
    LocalWalletDiffers {} -> "LocalWalletDiffers"
    LocalWalletsDiffers {} -> "LocalWalletsDiffers"
    AccountBalanceNotZero {} -> "AccountBalanceNotZero"
    LocalAccountDiffers {} -> "LocalAccountDiffers"
    LocalAccountsDiffers {} -> "LocalAccountsDiffers"
    AddressBalanceNotZero {} -> "AddressBalanceNotZero"
    LocalAddressesDiffer {} -> "LocalAddressesDiffer"
    LocalAddressDiffer {} -> "LocalAddressDiffer"
    InvalidTransactionState {} -> "InvalidTransactionState"
    InvalidTransactionFee {} -> "InvalidTransactionFee"
    LocalTransactionsDiffer {} -> "LocalTransactionsDiffer"
    LocalTransactionMissing {} -> "LocalTransactionMissing"

instance Exception WalletTestError


instance Buildable WalletTestError where
    build (HttpClientError _        )     = bprint "Http client error"
    build (WalletBalanceNotZero    w)     = bprint ("Wallet balance is not zero - ("%stext%")") (show w)
    build (WalletPassMissing       w)     = bprint ("Missing wallet pass - ("%stext%")") (show w)
    build (LocalWalletDiffers      w w')     = bprint ("Local wallet differs - ("%stext%"), ("%stext%")") (show w) (show w')
    build (LocalWalletsDiffers     w w')  = bprint ("Local wallets differs - ("%stext%"), ("%stext%")") (show w) (show w')

    build (AccountBalanceNotZero   a)     = bprint ("Acccount balance is not zero - ("%stext%")") (show a)
    build (LocalAccountDiffers     a a')  = bprint ("Local account differs - ("%stext%"), ("%stext%")") (show a) (show a')
    build (LocalAccountsDiffers    a a')  = bprint ("Local accounts differs - ("%stext%"), ("%stext%")") (show a) (show a')

    build (AddressBalanceNotZero   a)     = bprint ("Address balance is not zero - ("%stext%")") (show a)
    build (LocalAddressesDiffer a as)     = bprint ("Local address ("%stext%") missing from addresses ("%stext%")") (show a) (show as)
    build (LocalAddressDiffer      a a')  = bprint ("Local address differs - ("%stext%"), ("%stext%")") (show a) (show a')

    build (InvalidTransactionState t)     = bprint ("Transaction state is invalid. Transaction - ("%stext%")") (show t)
    build (InvalidTransactionFee   f)     = bprint ("Transaction fees are invalid - ("%stext%")") (show f)
    build (LocalTransactionsDiffer t t')  = bprint ("Local transactions differs - ("%stext%"), ("%stext%")") (show t) (show t')
    build (LocalTransactionMissing t ts)  = bprint ("Local transaction ("%stext%") missing from txs history ("%stext%")") (show t) (show ts)

