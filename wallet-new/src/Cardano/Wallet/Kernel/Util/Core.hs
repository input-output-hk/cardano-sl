-- | Utility functions on core types
--
-- Intended for qualified import
--
-- > import qualified Cardano.Wallet.Kernel.Util.Core as Core
module Cardano.Wallet.Kernel.Util.Core (
    -- * General utility functions
    getCurrentTimestamp
    -- * UTxO
  , utxoBalance
  , utxoRestrictToInputs
  , utxoRemoveInputs
  , utxoUnions
    -- * Transactions
  , paymentAmount
  , txOuts
  , txIns
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Serokell.Util (enumerate)

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (hash)

import           Cardano.Wallet.Kernel.Util

{-------------------------------------------------------------------------------
  General-utility functions
-------------------------------------------------------------------------------}

-- | Get current timestamp
--
-- NOTE: we are abandoning the 'Mockable time' strategy of core.
getCurrentTimestamp :: IO Core.Timestamp
getCurrentTimestamp = Core.Timestamp . round . (* 1000000) <$> getPOSIXTime

{-------------------------------------------------------------------------------
  UTxO
-------------------------------------------------------------------------------}

-- | Computes the balance for this 'Utxo'. We use 'unsafeAddCoin' as
-- as long as the 'maxCoinVal' stays within the 'Word64' 'maxBound', the
-- circulating supply of coins is finite and as such we should never have
-- to sum an 'Utxo' which would exceed the bounds.
-- If it does, this is clearly a bug and we throw an 'ErrorCall' exception
-- (crf. 'unsafeAddCoin' implementation).
utxoBalance :: Core.Utxo -> Core.Coin
utxoBalance = foldl' updateFn (Core.mkCoin 0) . Map.elems
  where
    updateFn :: Core.Coin -> Core.TxOutAux -> Core.Coin
    updateFn acc txOutAux =
      fromMaybe (error "utxoBalance: overflow") $
         Core.addCoin acc (toCoin txOutAux)

-- | Restricts the 'Utxo' to only the selected set of inputs.
utxoRestrictToInputs :: Core.Utxo -> Set Core.TxIn -> Core.Utxo
utxoRestrictToInputs = restrictKeys

utxoRemoveInputs :: Core.Utxo -> Set Core.TxIn -> Core.Utxo
utxoRemoveInputs = withoutKeys

utxoUnions :: [Core.Utxo] -> Core.Utxo
utxoUnions = Map.unions

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

-- | Calculates the amount of a requested payment.
paymentAmount :: NonEmpty Core.TxOut -> Core.Coin
paymentAmount = Core.unsafeIntegerToCoin
              . Core.sumCoins
              . map Core.txOutValue
              . toList

txOuts :: Core.Tx -> Core.Utxo
txOuts tx = Map.fromList $ map (toTxInOut (hash tx)) (outs tx)

txIns :: Core.TxAux -> Set Core.TxIn
txIns = Set.fromList . NE.toList . Core._txInputs . Core.taTx

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Gets the underlying value (as a 'Coin') from a 'TxOutAux'.
toCoin :: Core.TxOutAux -> Core.Coin
toCoin = Core.txOutValue . Core.toaOut

outs :: Core.Tx -> [(Word32, Core.TxOut)]
outs tx = enumerate $ toList $ tx ^. Core.txOutputs

toTxInOut :: Core.TxId -> (Word32, Core.TxOut) -> (Core.TxIn, Core.TxOutAux)
toTxInOut txId (idx, out) = (Core.TxInUtxo txId idx, Core.TxOutAux out)
