module Cardano.Wallet.Kernel.CoinSelection.Types (
    ExpenseRegulation (..)
    ) where

data ExpenseRegulation =
    SenderPaysFee
  | ReceiverPaysFee
