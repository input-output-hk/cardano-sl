{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Definitions of the most fundamental types.

module Pos.Types.Types
       (
         TxAttributes
       , TxInWitness (..)
       , TxWitness
       , TxDistribution (..)
       , TxSigData
       , TxSig
       , TxId
       , TxIn (..)
       , toPair
       , TxOut (..)
       , txOutStake
       , Tx (..)
       , _txInputs
       , _txOutputs
       , _txAttributes
       , txF
       , txaF
       , TxAux
       , TxOutAux

       , Utxo
       , formatUtxo
       , utxoF

       , TxUndo
       , TxsUndo

       , SharedSeed (..)
       , SlotLeaders

       , ProxySigLight
       , ProxySKLight
       , ProxySigHeavy
       , ProxySKHeavy
       , ProxySKEither
       ) where

import           Control.Lens           (makeLensesFor)
import           Data.DeriveTH          (derive, makeNFData)
import           Data.Hashable          (Hashable)
import qualified Data.Map               as M (toList)
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Data.Vector            (Vector)
import           Formatting             (Format, bprint, build, int, later, (%))
import           Serokell.AcidState     ()
import           Serokell.Util.Base16   (base16F, formatBase16)
import           Serokell.Util.Text     (listBuilderJSON, listJson, listJsonIndent,
                                         mapBuilderJson, pairBuilder)
import           Universum

import           Pos.Binary.Class       (Bi)
import           Pos.Crypto             (Hash, ProxySecretKey, ProxySignature, PublicKey,
                                         Signature, hash, shortHashF)
import           Pos.Data.Attributes    (Attributes)
import           Pos.Types.Address      ()
import           Pos.Types.Core         (Address (..), Coin, EpochIndex, StakeholderId,
                                         coinF)
import           Pos.Types.Script       (Script)

----------------------------------------------------------------------------
-- Transaction
----------------------------------------------------------------------------

-- | Represents transaction attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending transaction with new
-- fields via softfork.
type TxAttributes = Attributes ()

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

-- | Data that is being signed when creating a TxSig.
type TxSigData = (TxId, Word32, Hash [TxOut], Hash TxDistribution)

-- | 'Signature' of addrId.
type TxSig = Signature TxSigData

-- | A witness for a single input.
data TxInWitness
    = PkWitness { twKey :: !PublicKey
                , twSig :: !TxSig }
    | ScriptWitness { twValidator :: !Script
                    , twRedeemer  :: !Script }
    | UnknownWitnessType !Word8 !ByteString
    deriving (Eq, Show, Generic, Typeable)

instance Hashable TxInWitness

instance (Bi Script, Bi PublicKey) => Buildable TxInWitness where
    build (PkWitness key sig) =
        bprint ("PkWitness: key = "%build%", sig = "%build) key sig
    build (ScriptWitness val red) =
        bprint ("ScriptWitness: "%
                "validator hash = "%shortHashF%", "%
                "redeemer hash = "%shortHashF) (hash val) (hash red)
    build (UnknownWitnessType t bs) =
        bprint ("UnknownWitnessType "%build%" "%base16F) t bs

-- | A witness is a proof that a transaction is allowed to spend the funds it
-- spends (by providing signatures, redeeming scripts, etc). A separate proof
-- is provided for each input.
type TxWitness = Vector TxInWitness

-- | Distribution of “fake” stake that follow-the-satoshi would use for a
-- particular transaction.
newtype TxDistribution = TxDistribution {
    getTxDistribution :: [[(StakeholderId, Coin)]] }
    deriving (Eq, Show, Generic, Typeable)

instance Buildable TxDistribution where
    build (TxDistribution x) =
        listBuilderJSON . map (listBuilderJSON . map pairBuilder) $ x

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

-- | Make pair from TxIn
toPair :: TxIn -> (TxId, Word32)
toPair (TxIn h i) = (h, i)

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance Bi Address => Hashable TxOut

instance Bi Address => Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" -> "%build) txOutValue txOutAddress

type TxOutAux = (TxOut, [(StakeholderId, Coin)])

instance Bi Address => Buildable TxOutAux where
    build (out, distr) =
        bprint ("{txout = "%build%", distr = "%listJson%"}")
               out (map pairBuilder distr)

-- | Use this function if you need to know how a 'TxOut' distributes stake
-- (e.g. for the purpose of running follow-the-satoshi).
txOutStake :: TxOutAux -> [(StakeholderId, Coin)]
txOutStake (TxOut{..}, mb) = case txOutAddress of
    PubKeyAddress x _ -> [(x, txOutValue)]
    _                 -> mb

-- | Transaction.
--
-- NB: transaction witnesses are stored separately.
data Tx = Tx
    { txInputs     :: ![TxIn]   -- ^ Inputs of transaction.
    , txOutputs    :: ![TxOut]  -- ^ Outputs of transaction.
    , txAttributes :: !TxAttributes -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)

makeLensesFor [("txInputs", "_txInputs"), ("txOutputs", "_txOutputs")
              , ("txAttributes", "_txAttributes")] ''Tx

-- | Transaction + auxiliary data
type TxAux = (Tx, TxWitness, TxDistribution)

instance Bi Address => Hashable Tx

instance Bi Address => Buildable Tx where
    build Tx {..} =
        bprint
            ("Transaction with inputs "%listJson%", outputs: "%listJson)
            txInputs txOutputs

-- | Specialized formatter for 'Tx'.
txF :: Bi Address => Format r (Tx -> r)
txF = build

-- | Specialized formatter for 'Tx' with auxiliary data
txaF :: (Bi PublicKey, Bi Script, Bi Address) => Format r (TxAux -> r)
txaF = later $ \(tx, w, d) ->
    bprint (build%"\n"%
            "witnesses: "%listJsonIndent 4%"\n"%
            "distribution: "%build) tx w d

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
--
-- Transaction inputs are identified by (transaction ID, index in list of
-- output) pairs.
type Utxo = Map (TxId, Word32) TxOutAux

-- | Format 'Utxo' map as json.
formatUtxo :: Bi Address => Utxo -> Builder
formatUtxo =
    mapBuilderJson .
    map (first pairBuilder) .
    M.toList

-- | Specialized formatter for 'Utxo'.
utxoF :: Bi Address => Format r (Utxo -> r)
utxoF = later formatUtxo

----------------------------------------------------------------------------
-- UNDO
----------------------------------------------------------------------------

-- | Particular undo needed for transactions
type TxUndo = [TxOutAux]

type TxsUndo = [TxUndo]

----------------------------------------------------------------------------
-- SSC. It means shared seed computation, btw
----------------------------------------------------------------------------

-- | This is a shared seed used for follow-the-satoshi. This seed is
-- randomly generated by each party and eventually they agree on the
-- same value.
newtype SharedSeed = SharedSeed
    { getSharedSeed :: ByteString
    } deriving (Show, Eq, Ord, Generic, NFData, Typeable)

instance Buildable SharedSeed where
    build = formatBase16 . getSharedSeed

-- | 'NonEmpty' list of slot leaders.
type SlotLeaders = NonEmpty StakeholderId

----------------------------------------------------------------------------
-- Proxy signatures and delegation
----------------------------------------------------------------------------

-- | Proxy signature used in csl -- holds a pair of epoch
-- indices. Block is valid if it's epoch index is inside this range.
type ProxySigLight a = ProxySignature (EpochIndex, EpochIndex) a

-- | Same alias for the proxy secret key (see 'ProxySigLight').
type ProxySKLight = ProxySecretKey (EpochIndex, EpochIndex)

-- | Simple proxy signature without ttl/epoch index
-- constraints. 'EpochIndex' inside is needed for replay attack
-- prevention.
type ProxySigHeavy a = ProxySignature EpochIndex a

-- | Correspondent SK for no-ttl proxy signature scheme.
type ProxySKHeavy = ProxySecretKey EpochIndex

-- | Some proxy secret key.
type ProxySKEither = Either ProxySKLight ProxySKHeavy

derive makeNFData ''TxIn
derive makeNFData ''TxInWitness
derive makeNFData ''TxOut
derive makeNFData ''TxDistribution
derive makeNFData ''Tx
