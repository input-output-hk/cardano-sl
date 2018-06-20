-- | UPDATE operations on the wallet-spec state
module Cardano.Wallet.Kernel.DB.Spec.Util (
    PendingTxs
  , Balance
  , available
  , balance
  , balanceI
  , disjoint
  , isValidPendingTx
  , pendingUtxo
  , txAuxInputSet
  , txIns
  , utxoInputs
  , unionTxOuts
  , utxoRemoveInputs
  , utxoRestrictToInputs
  ) where

import           Universum

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

import qualified Pos.Core as Core

import           Pos.Txp (Utxo)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxIn (..), TxOut (..), TxOutAux (..))

import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.Types (txUtxo)

utxoOutputs :: Utxo -> [TxOut]
utxoOutputs = map toaOut . Map.elems

balanceI :: Utxo -> Balance
balanceI = Core.sumCoins . map txOutValue . utxoOutputs

balance :: Utxo -> Core.Coin
balance utxo
    = case Core.integerToCoin . balanceI $ utxo of
        Left _  -> error "balance' integerToCoin failed"
        Right c -> c

unionTxOuts :: [Utxo] -> Utxo
unionTxOuts = Map.unions

utxoInputs :: Utxo -> Set TxIn
utxoInputs = Map.keysSet

txIns :: PendingTxs -> Set TxIn
txIns = Set.fromList . concatMap (NE.toList . _txInputs . taTx) . Map.elems

txAuxInputSet :: TxAux -> Set TxIn
txAuxInputSet = Set.fromList . NE.toList . _txInputs . taTx

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys` s = m `Map.difference` Map.fromSet (const ()) s

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
m `restrictKeys` s = m `Map.intersection` Map.fromSet (const ()) s

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null (a `Set.intersection` b)

utxoRemoveInputs :: Utxo -> Set TxIn -> Utxo
utxoRemoveInputs = withoutKeys

utxoRestrictToInputs :: Utxo -> Set TxIn -> Utxo
utxoRestrictToInputs = restrictKeys

available :: Utxo -> PendingTxs -> Utxo
available utxo pending = utxoRemoveInputs utxo (txIns pending)

pendingUtxo :: PendingTxs -> Utxo
pendingUtxo pending = unionTxOuts $ map (txUtxo . taTx) $ Map.elems pending

isValidPendingTx :: TxAux -> Utxo -> Bool
isValidPendingTx tx availableUtxo
    = txInputs `Set.isSubsetOf` availableInputs
    where
        txInputs        = txAuxInputSet tx
        availableInputs = utxoInputs availableUtxo
