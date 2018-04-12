module Cardano.Wallet.Kernel.Types (
    -- * Input resolution
    -- ** Raw types
    ResolvedInput
  , ResolvedTxInputs
  , ResolvedBlockInputs
  , RawResolvedTx
  , RawResolvedBlock
    -- ** Derived types
  , ResolvedTx(..)
  , ResolvedBlock(..)
    -- ** From raw to derived types
  , fromRawResolvedTx
  , fromRawResolvedBlock
  ) where

import qualified Data.Map.Strict as Map
import           Data.Word (Word32)
import           Universum

import           Pos.Core (MainBlock, Tx, TxAux (..), TxIn (..), TxOut, TxOutAux (..), gbBody,
                           mbTxs, mbWitnesses, txInputs, txOutputs)
import           Pos.Crypto.Hashing (hash)
import           Pos.Txp (Utxo)
import           Serokell.Util (enumerate)

{-------------------------------------------------------------------------------
  Input resolution: raw types

  The raw types are the original types along with some additional information.
  In the derived types (below) we actually lose the original types (and
  therefore signatures etc.).
-------------------------------------------------------------------------------}

-- | Resolved input
--
-- A transaction input @(h, i)@ points to the @i@th output of the transaction
-- with hash @h@, which is not particularly informative. The corresponding
-- 'ResolvedInput' is obtained by looking up what that output actually is.
type ResolvedInput = TxOutAux

-- | All resolved inputs of a transaction
type ResolvedTxInputs = [ResolvedInput]

-- | All resolved inputs of a block
type ResolvedBlockInputs = [ResolvedTxInputs]

-- | Signed transaction along with its resolved inputs
--
-- Invariant: number of inputs @==@ number of resolved inputs
type RawResolvedTx = (TxAux, ResolvedTxInputs)

-- | Signed block along with its resolved inputs
--
-- Invariant: number of transactions @==@ number of resolved transaction inputs
type RawResolvedBlock = (MainBlock, ResolvedBlockInputs)

{-------------------------------------------------------------------------------
  Input resolution: derived types
-------------------------------------------------------------------------------}

-- | (Unsigned) transaction with inputs resolved
data ResolvedTx = ResolvedTx {
      -- | Transaction inputs
      rtxInputs  :: [(TxIn, ResolvedInput)]

      -- | Transaction outputs
    , rtxOutputs :: Utxo
    }

-- | (Unsigned block) containing resolved transactions
data ResolvedBlock = ResolvedBlock {
      -- | Transactions in the block
      rbTxs :: [ResolvedTx]
    }

{-------------------------------------------------------------------------------
  Construct derived types from raw types
-------------------------------------------------------------------------------}

fromRawResolvedTx :: RawResolvedTx -> ResolvedTx
fromRawResolvedTx (txAux, resolvedInputs) = ResolvedTx {
      rtxInputs  = zip inps resolvedInputs
    , rtxOutputs = Map.fromList $ map toTxInOut outs
    }
  where
    tx :: Tx
    tx = taTx txAux

    inps :: [TxIn]
    inps = toList $ tx ^. txInputs

    outs :: [(Word32, TxOut)]
    outs = enumerate $ toList $ tx ^. txOutputs

    toTxInOut :: (Word32, TxOut) -> (TxIn, TxOutAux)
    toTxInOut (idx, out) = (TxInUtxo (hash tx) idx, TxOutAux out)

fromRawResolvedBlock :: RawResolvedBlock -> ResolvedBlock
fromRawResolvedBlock (block, resolvedTxInputs) = ResolvedBlock {
      rbTxs = zipWith (curry fromRawResolvedTx) 
                (getBlockTxs block)
                resolvedTxInputs
    }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getBlockTxs :: MainBlock -> [TxAux]
getBlockTxs b = zipWith TxAux (b ^. gbBody ^. mbTxs)
                              (b ^. gbBody ^. mbWitnesses)
