{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
-- | Utility functions on core types
--
-- Intended for qualified import
--
-- > import qualified Cardano.Wallet.Kernel.Util.Core as Core
module Cardano.Wallet.Kernel.Util.Core (
    -- * General utility functions
    absCoin
  , derefIn
  , getCurrentTimestamp
  , fromUtxo
  , nothingToZero
  , sumCoinsUnsafe
  , toOutPair
    -- * UTxO
  , utxoBalance
  , utxoRestrictToInputs
  , utxoRemoveInputs
  , utxoUnions
  , toAddress
    -- * Transactions
  , paymentAmount
  , toCoin
  , txOuts
  , txIns
  , txAuxId
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

import           UTxO.Util

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

sumCoinsUnsafe :: (Container coins, Element coins ~ Core.Coin)
         => coins -> Core.Coin
sumCoinsUnsafe = Core.unsafeIntegerToCoin . Core.sumCoins

-- | This is not unsafe although we use unsafeGetCoin, because
-- this is not actually unsafe either.
absCoin :: Core.Coin -> Core.Coin -> Core.Coin
absCoin ca cb
    | a >= b = Core.Coin (a-b)
    | otherwise = Core.Coin (b-a)
    where
      a = Core.unsafeGetCoin ca
      b = Core.unsafeGetCoin cb

nothingToZero :: Ord a => a -> Map a Core.Coin -> Core.Coin
nothingToZero acc mp = case Map.lookup acc mp of
    Nothing -> Core.unsafeIntegerToCoin 0
    Just n  -> n

-- | Gets the underlying value (as a 'Coin') from a 'TxOutAux'.
toCoin :: Core.TxOutAux -> Core.Coin
toCoin = Core.txOutValue . Core.toaOut

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

-- | Gets the underlying address from a 'TxOutAux'.
toAddress :: Core.TxOutAux -> Core.Address
toAddress = Core.txOutAddress . Core.toaOut

outs :: Core.Tx -> [(Word32, Core.TxOut)]
outs tx = enumerate $ toList $ tx ^. Core.txOutputs

toTxInOut :: Core.TxId -> (Word32, Core.TxOut) -> (Core.TxIn, Core.TxOutAux)
toTxInOut txId (idx, out) = (Core.TxInUtxo txId idx, Core.TxOutAux out)
