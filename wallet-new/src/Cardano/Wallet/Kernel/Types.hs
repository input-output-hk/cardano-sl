module Cardano.Wallet.Kernel.Types (
    -- * Input resolution
    -- ** Raw types
    ResolvedInput
  , ResolvedTxInputs
  , ResolvedBlockInputs
  , RawResolvedTx(..)
  , invRawResolvedTx
  , mkRawResolvedTx
  , RawResolvedBlock(..)
  , invRawResolvedBlock
  , mkRawResolvedBlock
    -- ** Derived types
  , ResolvedTx(..)
  , ResolvedBlock(..)
    -- ** From raw to derived types
  , fromRawResolvedTx
  , fromRawResolvedBlock
  , txUtxo
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Word (Word32)

import           Pos.Core (MainBlock, Tx, TxAux (..), TxIn (..), TxOut, TxOutAux (..), gbBody,
                           mbTxs, mbWitnesses, txInputs, txOutputs)
import           Pos.Crypto.Hashing (hash)
import           Pos.Txp (Utxo)
import           Serokell.Util (enumerate)

{-------------------------------------------------------------------------------
  Input resolution: raw types
-------------------------------------------------------------------------------}

-- | Resolved input
--
-- A transaction input @(h, i)@ points to the @i@th output of the transaction
-- with hash @h@, which is not particularly informative. The corresponding
-- 'ResolvedInput' is obtained by looking up what that output actually is.
type ResolvedInput = TxOutAux

-- | All resolved inputs of a transaction
type ResolvedTxInputs = NonEmpty ResolvedInput

-- | All resolved inputs of a block
type ResolvedBlockInputs = [ResolvedTxInputs]

-- | Signed transaction along with its resolved inputs
--
-- Constructor is marked as unsafe because the caller should make sure that
-- invariant 'invRawResolvedTx' holds.
data RawResolvedTx = UnsafeRawResolvedTx {
      rawResolvedTx       :: TxAux
    , rawResolvedTxInputs :: ResolvedTxInputs
    }

-- | Invariant for 'RawResolvedTx'
--
-- > number of inputs @==@ number of resolved inputs
invRawResolvedTx :: TxAux -> ResolvedTxInputs -> Bool
invRawResolvedTx txAux ins = length (taTx txAux ^. txInputs) == length ins

-- | Smart constructor for 'RawResolvedTx' that checks the invariant
mkRawResolvedTx :: TxAux -> ResolvedTxInputs -> RawResolvedTx
mkRawResolvedTx txAux ins =
    if invRawResolvedTx txAux ins
      then UnsafeRawResolvedTx txAux ins
      else error "mkRawResolvedTx: invariant violation"

-- | Signed block along with its resolved inputs
--
-- Constructor is marked unsafe because the caller should make sure that
-- invariant 'invRawResolvedBlock' holds.
data RawResolvedBlock = UnsafeRawResolvedBlock {
      rawResolvedBlock       :: MainBlock
    , rawResolvedBlockInputs :: ResolvedBlockInputs
    }

-- | Invariant for 'RawResolvedBlock'
--
-- > number of transactions @==@ number of resolved transaction inputs
--
-- Moreover, 'invRawResolvedTx' should hold for each transaction.
invRawResolvedBlock :: MainBlock -> ResolvedBlockInputs -> Bool
invRawResolvedBlock block ins =
       length txs == length ins
    && all (uncurry invRawResolvedTx) (zip txs ins)
  where
    txs :: [TxAux]
    txs = getBlockTxs block

-- | Smart constructor for 'RawResolvedBlock' that checks the invariant
mkRawResolvedBlock :: MainBlock -> ResolvedBlockInputs -> RawResolvedBlock
mkRawResolvedBlock block ins =
    if invRawResolvedBlock block ins
      then UnsafeRawResolvedBlock block ins
      else error "mkRawResolvedBlock: invariant violation"

{-------------------------------------------------------------------------------
  Input resolution: derived types
-------------------------------------------------------------------------------}

-- | (Unsigned) transaction with inputs resolved
--
-- NOTE: We cannot recover the original transaction from a 'ResolvedTx'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedTx = ResolvedTx {
      -- | Transaction inputs
      rtxInputs  :: NonEmpty (TxIn, ResolvedInput)

      -- | Transaction outputs
    , rtxOutputs :: Utxo
    }

-- | (Unsigned block) containing resolved transactions
--
-- NOTE: We cannot recover the original block from a 'ResolvedBlock'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedBlock = ResolvedBlock {
      -- | Transactions in the block
      rbTxs :: [ResolvedTx]
    }

{-------------------------------------------------------------------------------
  Construct derived types from raw types
-------------------------------------------------------------------------------}

fromRawResolvedTx :: RawResolvedTx -> ResolvedTx
fromRawResolvedTx rtx = ResolvedTx {
      rtxInputs  = NE.zip inps (rawResolvedTxInputs rtx)
    , rtxOutputs = txUtxo tx
    }
  where
    tx :: Tx
    tx = taTx (rawResolvedTx rtx)

    inps :: NonEmpty TxIn
    inps = tx ^. txInputs

txUtxo :: Tx -> Utxo
txUtxo tx = Map.fromList $
                map (toTxInOut tx) (outs tx)

outs :: Tx -> [(Word32, TxOut)]
outs tx = enumerate $ toList $ tx ^. txOutputs

toTxInOut :: Tx -> (Word32, TxOut) -> (TxIn, TxOutAux)
toTxInOut tx (idx, out) = (TxInUtxo (hash tx) idx, TxOutAux out)

fromRawResolvedBlock :: RawResolvedBlock -> ResolvedBlock
fromRawResolvedBlock rb = ResolvedBlock {
      rbTxs = zipWith aux (getBlockTxs (rawResolvedBlock rb))
                          (rawResolvedBlockInputs rb)
    }
  where
    -- Justification for the use of the unsafe constructor:
    -- The invariant for 'RawResolvedBlock' guarantees the invariant for the
    -- individual transactions.
    aux :: TxAux -> ResolvedTxInputs -> ResolvedTx
    aux txAux ins = fromRawResolvedTx $ UnsafeRawResolvedTx txAux ins

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getBlockTxs :: MainBlock -> [TxAux]
getBlockTxs b = zipWith TxAux (b ^. gbBody ^. mbTxs)
                              (b ^. gbBody ^. mbWitnesses)
