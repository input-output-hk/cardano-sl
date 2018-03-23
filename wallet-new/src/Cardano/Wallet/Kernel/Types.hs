module Cardano.Wallet.Kernel.Types (
     ResolvedTx(..)
   , ResolvedBlock (..)
   , RawResolvedTx
   , ResolvedTxPair
   , mkResolvedBlock
   , txUtxo
  ) where

import           Universum
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Serokell.Util (enumerate)
import           Pos.Crypto (WithHash (..), withHash)
import           Pos.Core.Txp (TxId, Tx (..), TxIn (..), TxOutAux (..), TxOut, TxAux)
import           Pos.Core.Block (Block)
import           Pos.Core (gbBody, mbTxPayload, txpTxs)
import           Pos.Txp.Toil(Utxo)

-- TODO Docs
-- TODO smart constructors that express invariants

type RawResolvedTx  = (TxAux, [TxOutAux])

type ResolvedTxPair = (TxIn, TxOutAux)

data ResolvedTx = ResolvedTx {
    rtxInputs  :: [ResolvedTxPair]
  , rtxOutputs :: Utxo
}

data ResolvedBlock = ResolvedBlock {
   rbTxs :: [ResolvedTx]
 , rbBlock :: Block
}

{-------------------------------------------------------------------------------
  Convert Blund to ResolvedBlock, ResolvedBlock to [ResolvedTx]
-------------------------------------------------------------------------------}

mkResolvedBlock :: Block -> [[TxOutAux]] -> ResolvedBlock
mkResolvedBlock b spentOutputs
    = ResolvedBlock{..}
    where
        rbTxs = zipWith toResolvedTx (getBlockTxs b) spentOutputs
        rbBlock = b

txOutUtxo :: TxId -> [TxOut] -> Utxo
txOutUtxo txId outputs
    = Map.fromList $ map toTxInOut $ enumerate outputs

      where toTxInOut (idx, out) = (TxInUtxo txId idx, TxOutAux out)

unpackTx :: Tx -> ([TxIn], Utxo)
unpackTx tx
    = let (WithHash tx' txId) = withHash tx
          (UnsafeTx (NE.toList -> inputs) (NE.toList -> outputs) _) = tx' in

      (inputs, txOutUtxo txId outputs)

txUtxo :: Tx -> Utxo
txUtxo = snd . unpackTx

toResolvedTx :: Tx -> [TxOutAux] -> ResolvedTx
toResolvedTx tx spentOutputs
    = let (inputs, outUtxo) = unpackTx tx
          rtxInputs = zip inputs spentOutputs
          rtxOutputs = outUtxo in

      ResolvedTx{..}

getBlockTxs :: Block -> [Tx]
getBlockTxs b
      = b' ^. gbBody ^. mbTxPayload ^. txpTxs
         where b' = case b of
                       Left _ -> error "genesis block, expecting a MainBlock"
                       Right mainBlock -> mainBlock
