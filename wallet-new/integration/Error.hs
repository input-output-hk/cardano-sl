{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types describing runtime errors related to
-- wallet integration tests.
module Error
    ( WalletTestError (..)
    , showConstr
    ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))

import           Cardano.Wallet.API.V1.Types (Account, Address, EstimatedFees, Transaction, V1,
                                              Wallet, WalletAddress)

import           Cardano.Wallet.Client (ClientError)
import qualified Pos.Core as Core


data WalletTestError
    = HttpClientError ClientError

    | WalletBalanceNotZero Wallet
    | WalletPassMissing Wallet
    | LocalWalletDiffers Wallet Wallet
    | LocalWalletsDiffers [Wallet] [Wallet]

    | AccountBalanceNotZero Account
    | LocalAccountDiffers Account Account
    | LocalAccountsDiffers [Account] [Account]

    | LocalAddressesDiffer [WalletAddress] [WalletAddress]
    | LocalAddressDiffer Address Address

    | InvalidTransactionState Transaction
    | InvalidTransactionFee EstimatedFees
    | UnexpectedChangeAddress [WalletAddress]
    | UnexpectedAccountBalance Text (V1 Core.Coin) (V1 Core.Coin)
    | CantFindAddress (V1 Core.Address)
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
    LocalAddressesDiffer {} -> "LocalAddressesDiffer"
    LocalAddressDiffer {} -> "LocalAddressDiffer"
    InvalidTransactionState {} -> "InvalidTransactionState"
    InvalidTransactionFee {} -> "InvalidTransactionFee"
    UnexpectedChangeAddress {} -> "UnexpectedChangeAddress"
    UnexpectedAccountBalance explanation _ _ ->
        toString ("UnexpectedAccountBalance " <> explanation)
    CantFindAddress {} -> "CantFindAddress"
    LocalTransactionsDiffer {} -> "LocalTransactionsDiffer"
    LocalTransactionMissing {} -> "LocalTransactionMissing"

instance Exception WalletTestError


instance Buildable WalletTestError where
    build (HttpClientError _        )     = bprint "Http client error"
    build (WalletBalanceNotZero    w)     = bprint ("Wallet balance is not zero - ("%stext%")") (show w)
    build (WalletPassMissing       w)     = bprint ("Missing wallet pass - ("%stext%")") (show w)
    build (LocalWalletDiffers      w w')  = bprint ("Local wallet differs - ("%stext%"), ("%stext%")") (show w) (show w')
    build (LocalWalletsDiffers     w w')  = bprint ("Local wallets differs - ("%stext%"), ("%stext%")") (show w) (show w')

    build (AccountBalanceNotZero   a)     = bprint ("Acccount balance is not zero - ("%stext%")") (show a)
    build (LocalAccountDiffers     a a')  = bprint ("Local account differs - ("%stext%"), ("%stext%")") (show a) (show a')
    build (LocalAccountsDiffers    a a')  = bprint ("Local accounts differs - ("%stext%"), ("%stext%")") (show a) (show a')

    build (LocalAddressesDiffer a as)     = bprint ("Local address ("%stext%") missing from addresses ("%stext%")") (show a) (show as)
    build (LocalAddressDiffer      a a')  = bprint ("Local address differs - ("%stext%"), ("%stext%")") (show a) (show a')

    build (InvalidTransactionState t)     = bprint ("Transaction state is invalid. Transaction - ("%stext%")") (show t)
    build (InvalidTransactionFee   f)     = bprint ("Transaction fees are invalid - ("%stext%")") (show f)
    build (UnexpectedChangeAddress a)     = bprint ("Unexpected change address after transaction ("%stext%")") (show a)
    build (UnexpectedAccountBalance explanation b a) =
        bprint ("Unexpected account balance "%stext%". Before ("%stext%"), after ("%stext%"))") explanation (show b) (show a)
    build (CantFindAddress a)             = bprint ("Can't find address ("%stext%") before and/or after transaction") (show a)
    build (LocalTransactionsDiffer t t')  = bprint ("Local transactions differs - ("%stext%"), ("%stext%")") (show t) (show t')
    build (LocalTransactionMissing t ts)  = bprint ("Local transaction ("%stext%") missing from txs history ("%stext%")") (show t) (show ts)

