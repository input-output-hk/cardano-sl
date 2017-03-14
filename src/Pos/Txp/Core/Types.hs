{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core types of Txp component, i. e. types which actually form
-- block or are used by other components.

module Pos.Txp.Core.Types
       ( TxId

       -- * Witness
       , TxInWitness (..)
       , TxWitness
       , TxDistribution (..)
       , TxSigData
       , TxSig

       -- * Tx parts
       , TxIn (..)
       , TxOut (..)
       , TxOutAux (..)
       , TxAttributes

       -- * Tx
       , Tx (..)
       , TxAux
       , mkTx
       , txInputs
       , txOutputs
       , txAttributes
       , txF
       , txaF

       -- * Payload and proof
       , TxProof (..)
       , TxPayload (..)
       , mkTxPayload
       , txpTxs
       , txpWitnesses
       , txpDistributions

       -- * Undo
       , TxUndo
       , TxpUndo
       ) where

import           Control.Lens         (makeLenses)
import           Data.DeriveTH        (derive, makeNFData)
import           Data.Hashable        (Hashable)
import           Data.Text.Buildable  (Buildable)
import qualified Data.Text.Buildable  as Buildable
import           Data.Vector          (Vector)
import           Formatting           (Format, bprint, build, formatToString, int, later,
                                       sformat, (%))
import           Serokell.Util.Base16 (base16F)
import           Serokell.Util.Text   (listBuilderJSON, listJson, listJsonIndent,
                                       pairBuilder)
import           Serokell.Util.Verify (VerificationRes (..), verResSingleF, verifyGeneric)
import           Universum

import           Pos.Binary.Class     (Bi)
import           Pos.Binary.Core      ()
import           Pos.Binary.Crypto    ()
import           Pos.Core.Address     ()
import           Pos.Core.Types       (Address (..), Coin, Script, StakeholderId, coinF,
                                       mkCoin)
import           Pos.Crypto           (Hash, PublicKey, RedeemPublicKey, RedeemSignature,
                                       Signature, hash, shortHashF)
import           Pos.Data.Attributes  (Attributes)
import           Pos.Merkle           (MerkleRoot, MerkleTree, mkMerkleTree)

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

----------------------------------------------------------------------------
-- Witness
----------------------------------------------------------------------------

-- | Data that is being signed when creating a TxSig.
type TxSigData = (TxId, Word32, Hash (NonEmpty TxOut), Hash TxDistribution)

-- | 'Signature' of addrId.
type TxSig = Signature TxSigData

-- | A witness for a single input.
data TxInWitness
    = PkWitness { twKey :: !PublicKey
                , twSig :: !TxSig }
    | ScriptWitness { twValidator :: !Script
                    , twRedeemer  :: !Script }
    | RedeemWitness { twRedeemKey :: !RedeemPublicKey
                    , twRedeemSig :: !(RedeemSignature TxSigData) }
    | UnknownWitnessType !Word8 !ByteString
    deriving (Eq, Show, Generic, Typeable)

instance Hashable TxInWitness

instance Buildable TxInWitness where
    build (PkWitness key sig) =
        bprint ("PkWitness: key = "%build%", sig = "%build) key sig
    build (ScriptWitness val red) =
        bprint ("ScriptWitness: "%
                "validator hash = "%shortHashF%", "%
                "redeemer hash = "%shortHashF) (hash val) (hash red)
    build (RedeemWitness key sig) =
        bprint ("PkWitness: key = "%build%", sig = "%build) key sig
    build (UnknownWitnessType t bs) =
        bprint ("UnknownWitnessType "%build%" "%base16F) t bs

-- | A witness is a proof that a transaction is allowed to spend the funds it
-- spends (by providing signatures, redeeming scripts, etc). A separate proof
-- is provided for each input.
type TxWitness = Vector TxInWitness

-- | Distribution of “fake” stake that follow-the-satoshi would use
-- for a particular transaction.  Length of stored list must be same
-- as length of '_txOutputs' of corresponding transaction.
newtype TxDistribution = TxDistribution
    { getTxDistribution :: NonEmpty [(StakeholderId, Coin)]
    } deriving (Eq, Show, Generic, Typeable)

instance Buildable TxDistribution where
    build (TxDistribution x) =
        listBuilderJSON . map (listBuilderJSON . map pairBuilder) $ x

----------------------------------------------------------------------------
-- Tx parts
----------------------------------------------------------------------------

-- | Transaction input.
data TxIn = TxIn
    { -- | Which transaction's output is used
      txInHash  :: !TxId
      -- | Index of the output in transaction's outputs
    , txInIndex :: !Word32
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Hashable TxIn

instance Buildable TxIn where
    build TxIn {..} = bprint ("TxIn "%shortHashF%" #"%int) txInHash txInIndex

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" -> "%build) txOutValue txOutAddress

-- | Transaction output and auxilary data corresponding to it.
-- [CSL-366] Add more data.
data TxOutAux = TxOutAux
    { toaOut   :: !TxOut                   -- ^ Tx output
    , toaDistr :: ![(StakeholderId, Coin)] -- ^ Stake distribution
                                           -- associated with output
    } deriving (Show, Eq)

instance Buildable TxOutAux where
    build (TxOutAux out distr) =
        bprint ("{txout = "%build%", distr = "%listJson%"}")
               out (map pairBuilder distr)

-- | Represents transaction attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending transaction with new
-- fields via softfork.
type TxAttributes = Attributes ()

----------------------------------------------------------------------------
-- Tx
----------------------------------------------------------------------------

-- | Transaction.
--
-- NB: transaction witnesses are stored separately.
data Tx = UnsafeTx
    { _txInputs     :: !(NonEmpty TxIn)  -- ^ Inputs of transaction.
    , _txOutputs    :: !(NonEmpty TxOut) -- ^ Outputs of transaction.
    , _txAttributes :: !TxAttributes     -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)

makeLenses ''Tx

-- | Transaction + auxiliary data
type TxAux = (Tx, TxWitness, TxDistribution)

instance Hashable Tx

instance Buildable Tx where
    build UnsafeTx {..} =
        bprint
            ("Transaction with inputs "%listJson%", outputs: "%listJson)
            _txInputs _txOutputs

-- | Specialized formatter for 'Tx'.
txF :: Format r (Tx -> r)
txF = build

-- | Specialized formatter for 'Tx' with auxiliary data
txaF :: Format r (TxAux -> r)
txaF = later $ \(tx, w, d) ->
    bprint (build%"\n"%
            "witnesses: "%listJsonIndent 4%"\n"%
            "distribution: "%build) tx w d

-- | Create valid Tx or fail.
-- Verify inputs and outputs are non empty; have enough coins.
mkTx
    :: MonadFail m
    => NonEmpty TxIn -> NonEmpty TxOut -> TxAttributes -> m Tx
mkTx inputs outputs attrs =
    case verRes of
        VerSuccess -> pure $ UnsafeTx inputs outputs attrs
        failure    -> fail $ formatToString verResSingleF failure
  where
    verRes =
        verifyGeneric $
        concat $ zipWith outputPredicates [0 ..] $ toList outputs
    outputPredicates (i :: Word) TxOut {..} =
        [ ( txOutValue > mkCoin 0
          , sformat
                ("output #"%int%" has non-positive value: "%coinF)
                i txOutValue)
        ]

----------------------------------------------------------------------------
-- Payload and proof
----------------------------------------------------------------------------

data TxProof = TxProof
    { txpNumber            :: !Word32
    , txpRoot              :: !(MerkleRoot Tx)
    , txpWitnessesHash     :: !(Hash [TxWitness])
    , txpDistributionsHash :: !(Hash [TxDistribution])
    } deriving (Show, Eq, Generic)

-- | Payload of Txp component which is part of main block. Constructor
-- is unsafe, because it lets one create invalid payload, for example
-- with different number of transactions and witnesses.
data TxPayload = UnsafeTxPayload
    { -- | Transactions are the main payload.
      _txpTxs           :: !(MerkleTree Tx)
    , -- | Transaction witnesses. Invariant: there are as many witnesses
      -- as there are transactions in the block. This is checked during
      -- deserialisation. We can't put them into the same Merkle tree
      -- with transactions, as the whole point of segwit is to separate
      -- transactions and witnesses.
      --
      -- TODO: should they be put into a separate Merkle tree or left as
      -- a list?
      _txpWitnesses     :: ![TxWitness]
    , -- | Distributions for P2SH addresses in transaction outputs.
      --     * length mbTxAddrDistributions == length mbTxs
      --     * i-th element is 'Just' if at least one output of i-th
      --         transaction is P2SH
      --     * n-th element of i-th element is 'Just' if n-th output
      --         of i-th transaction is P2SH
      -- Ask @neongreen if you don't understand wtf is going on.
      -- Basically, address distributions are needed so that (potential)
      -- receivers of P2SH funds would count as stakeholders.
      _txpDistributions :: ![TxDistribution]
    } deriving (Show, Eq, Generic)

makeLenses ''TxPayload

-- | Smart constructor of 'TxPayload' which can fail if some
-- invariants are violated.
--
-- Currently there are two invariants:
-- • number of txs must be same as number of witnesses and
--   number of distributions;
-- • each distribution must have the same number of elements as
--   number of outputs in corresponding transaction.
mkTxPayload
    :: (Bi Tx, MonadFail m)
    => [(Tx, TxWitness, TxDistribution)] -> m TxPayload
mkTxPayload txws = do
    let (txs, _txpWitnesses, _txpDistributions) = unzip3 txws
    let _txpTxs = mkMerkleTree txs
    let payload = UnsafeTxPayload {..}
    mapM_ checkLen (zip3 [0 ..] txs _txpDistributions) $> payload
  where
    checkLen
        :: MonadFail m
        => (Int, Tx, TxDistribution) -> m ()
    checkLen (i, tx, ds) = do
        let lenOut = length (_txOutputs tx)
            lenDist = length (getTxDistribution ds)
        when (lenOut /= lenDist) $ fail $
            formatToString
                ("mkTxPayload: "%"amount of outputs ("%int%") of tx "%
                 "#"%int%" /= amount of distributions " %
                 "for this tx ("%int%")")
                lenOut i lenDist

----------------------------------------------------------------------------
-- Undo
----------------------------------------------------------------------------

-- | Particular undo needed for transactions
type TxUndo = NonEmpty TxOutAux

type TxpUndo = [TxUndo]

----------------------------------------------------------------------------
-- TH instances
----------------------------------------------------------------------------

derive makeNFData ''TxIn
derive makeNFData ''TxInWitness
derive makeNFData ''TxOut
derive makeNFData ''TxOutAux
derive makeNFData ''TxDistribution
derive makeNFData ''Tx
derive makeNFData ''TxProof
derive makeNFData ''TxPayload
