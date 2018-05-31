module Cardano.Wallet.Kernel.CoinSelection.Types (
    ExpenseRegulation (..)
    ) where

data ExpenseRegulation =
    SenderPaysFees
  | ReceiverPaysFees
