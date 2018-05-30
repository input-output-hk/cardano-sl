module Cardano.Wallet.Kernel.CoinSelection.Types (
      ExpenseRegulation (..)
    , getRegulationRatio
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

-- | A value of 0 means that 0% of the fee will be payed by the recipient.
-- Conversely, a value of 1.0 means that the recipient will pay for the fee
-- entirely.
getRegulationRatio :: ExpenseRegulation -> Double
getRegulationRatio SenderPaysFees   = 0.0
getRegulationRatio ReceiverPaysFees = 1.0
