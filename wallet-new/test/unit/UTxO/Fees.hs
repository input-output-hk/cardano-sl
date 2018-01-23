-- | Fee calculation
--
-- See also
--
-- * Module "Pos.Core.Common.Fee"
-- * Function 'Pos.Client.Txp.Util.stabilizeTxFee'
module UTxO.Fees (
    Fee
  , calculateFee
  ) where

import Universum

import Pos.Core
import Pos.Client.Txp
import Pos.Txp.Toil

import UTxO.Context
import UTxO.DSL
import UTxO.Interpreter
import UTxO.Translate

{-------------------------------------------------------------------------------
  Fee calculation
-------------------------------------------------------------------------------}

type Fee = Value

-- | Calculate fee for a transaction
--
-- NOTE: The fee of a transaction depends on its size, leading to catch-22.
-- We therefore first pretend the fee is 0, compute the transaction size of the
-- interpreted transaction, and then return the properly constructed
-- transaction. THIS WILL BREAK if the structure of the transaction changes
-- depending on the fee we calculate here; use with caution!
calculateFee :: (Fee -> Transaction Addr)
             -> Translate IntException (Transaction Addr)
calculateFee f = do
    tx <- int (f 0)
    TxFeePolicyTxSizeLinear policy <- bvdTxFeePolicy <$> gsAdoptedBVData
    TxFee fee <- mapTranslateErrors IntExTx $ txToLinearFee policy tx
    return $ f (unsafeGetCoin fee)
