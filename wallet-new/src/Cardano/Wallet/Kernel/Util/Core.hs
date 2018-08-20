-- | Utility functions on core types
--
-- Intended for qualified import
--
-- > import qualified Cardano.Wallet.Kernel.Util.Core as Core
module Cardano.Wallet.Kernel.Util.Core (
    -- * General utility functions
    getCurrentTimestamp
  , derefIn
  , fromUtxo
  , toOutPair
  , getSomeTimestamp
    -- * UTxO
  , utxoBalance
  , utxoRestrictToInputs
  , utxoRemoveInputs
  , utxoUnions
    -- * Transactions
  , paymentAmount
  , txOuts
  , txIns
  , txAuxId
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units (fromMicroseconds)
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

getSomeTimestamp :: Core.Timestamp
getSomeTimestamp = Core.Timestamp $ fromMicroseconds 12340000

{-------------------------------------------------------------------------------
  UTxO
-------------------------------------------------------------------------------}

-- | Computes the balance for this UTxO
--
-- This returns an 'Integer' rather than a 'Coin' because the outputs of a
-- block may sum to more than 'maxCoinVal' (if some outputs of the transactions
-- in the block are used as inputs by other transactions in that block).
utxoBalance :: Core.Utxo -> Integer
utxoBalance = foldl' updateFn 0 . Map.elems
  where
    updateFn :: Integer -> Core.TxOutAux -> Integer
    updateFn acc txOut = acc + Core.coinToInteger (toCoin txOut)

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

txAuxId :: Core.TxAux -> Core.TxId
txAuxId = hash . Core.taTx

{-------------------------------------------------------------------------------
  External auxiliary
-------------------------------------------------------------------------------}

toOutPair :: Core.TxOutAux -> (Core.Address, Core.Coin)
toOutPair txOutAux = (toAddress txOutAux, toCoin txOutAux)

fromUtxo :: Core.Utxo -> Maybe (NE.NonEmpty (Core.Address, Core.Coin))
fromUtxo utxo = NE.nonEmpty $ toOutPair <$> Map.elems utxo

derefIn :: Core.TxIn -> Maybe (Core.TxId, Word32)
derefIn txIn = case txIn of
   Core.TxInUnknown _ _  -> Nothing
   Core.TxInUtxo txId ix -> Just (txId, ix)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Gets the underlying value (as a 'Coin') from a 'TxOutAux'.
toCoin :: Core.TxOutAux -> Core.Coin
toCoin = Core.txOutValue . Core.toaOut

-- | Gets the underlying address from a 'TxOutAux'.
toAddress :: Core.TxOutAux -> Core.Address
toAddress = Core.txOutAddress . Core.toaOut

outs :: Core.Tx -> [(Word32, Core.TxOut)]
outs tx = enumerate $ toList $ tx ^. Core.txOutputs

toTxInOut :: Core.TxId -> (Word32, Core.TxOut) -> (Core.TxIn, Core.TxOutAux)
toTxInOut txId (idx, out) = (Core.TxInUtxo txId idx, Core.TxOutAux out)
