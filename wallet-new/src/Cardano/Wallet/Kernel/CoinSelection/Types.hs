module Cardano.Wallet.Kernel.CoinSelection.Types (
      ExpenseRegulation
    , getRegulationRatio
    , senderPays
    , recipientPays
    , shareCost
    ) where

import           Universum

{-------------------------------------------------------------------------------
  Expense regulation
-------------------------------------------------------------------------------}

-- | An 'ExpenseRegulation' regulate how much of the fee the recipient
-- will need to pay. Despite modelled as a 'Double' underneath it can be
-- constructed via smart constructors only in the close range [0,1].
-- A value of 0 means that 0% of the fee will be payed by the recipient.
-- Conversely, a value of 1.0 means that the recipient will pay for the fee
-- entirely.
newtype ExpenseRegulation = ExpenseRegulation { getRegulationRatio :: Double } deriving Eq

-- | The send pays for the fees entirely.
senderPays :: ExpenseRegulation
senderPays = ExpenseRegulation 0

-- | The recipient pays for the fees entirely.
recipientPays :: ExpenseRegulation
recipientPays = ExpenseRegulation 1

-- | Both the sender and the recipient pays in equal amount for the fees
shareCost :: ExpenseRegulation
shareCost = ExpenseRegulation 0.5
