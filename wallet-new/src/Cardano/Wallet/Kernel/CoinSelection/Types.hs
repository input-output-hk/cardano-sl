module Cardano.Wallet.Kernel.CoinSelection.Types (
      ExpenseRegulation (..)
    ) where

import           Universum

{-------------------------------------------------------------------------------
  Expense regulation
-------------------------------------------------------------------------------}

-- | An 'ExpenseRegulation' regulate how much of the fee the recipient
-- will need to pay.
data ExpenseRegulation =
      SenderPaysFees
      -- ^ The send pays for the fees entirely.
    | ReceiverPaysFees
    -- ^ The recipient pays for the fees entirely.
    deriving Eq
