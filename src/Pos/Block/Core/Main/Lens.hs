{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Lenses for main blockchain types.

module Pos.Block.Core.Main.Lens
       ( mbMpc
       , mbTxPayload
       , mbTxs
       , mbWitnesses
       , mbProxySKs
       , mbUpdatePayload
       , mcdSlot
       , mcdLeaderKey
       , mcdDifficulty
       , mcdSignature

       , mehBlockVersion
       , mehSoftwareVersion
       , mehAttributes
       , mebAttributes

       , headerLeaderKey
       , headerSignature

       , blockLeaderKey
       , blockMpc
       , blockSignature
       , blockSlot
       , blockTxs
       , blockTxas
       , blockProxySKs
       ) where

import           Universum

import           Control.Lens               (Getter, makeLenses, to)
import           Data.List                  (zipWith3)

import           Pos.Block.Core.Main.Chain  (Body (..), ConsensusData (..))
import           Pos.Block.Core.Main.Types  (BlockSignature, MainBlock, MainBlockHeader,
                                             MainBlockchain, MainExtraBodyData,
                                             MainExtraHeaderData)
import           Pos.Block.Core.Union.Types (BiHeader)
import           Pos.Core                   (ChainDifficulty, ProxySKHeavy, SlotId,
                                             gbBody, gbHeader, gbhConsensus)
import           Pos.Crypto                 (PublicKey)
import           Pos.Merkle                 (MerkleTree)
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Txp.Core               (Tx, TxAux (..), TxDistribution, TxPayload,
                                             TxWitness, txpDistributions, txpTxs,
                                             txpWitnesses)
import           Pos.Update.Core.Types      (UpdatePayload)

-- -- ***TODO*** -- --
-- This comment and macros are copy-pasted and it's bad, but I
-- will either do something with it later or we will just use a
-- solution.
-- -- ***TODO*** -- --

-- !!! Create issue about this on lens github or give link on existing issue !!!
-- 'makeLensesData' doesn't work with types with parameters. I don't
-- know how to design a 'makeLensesData' which would work with them (in fact,
-- I don't even know how an invocation of 'makeLensesData' would look like)
--
-- UPDATE: the issue is https://github.com/ekmett/lens/issues/733

#define MAKE_LENS(l, field) l f s = (\y -> s {field = y}) <$> f (field s)

-- makeLensesData ''ConsensusData ''(MainBlockchain ssc)

-- | Lens for 'SlotId' of 'MainBlockchain' in 'ConsensusData'.
mcdSlot :: Lens' (ConsensusData (MainBlockchain ssc)) SlotId
MAKE_LENS(mcdSlot, _mcdSlot)

-- | Lens for 'PublicKey' of 'MainBlockchain' in 'ConsensusData'.
mcdLeaderKey :: Lens' (ConsensusData (MainBlockchain ssc)) PublicKey
MAKE_LENS(mcdLeaderKey, _mcdLeaderKey)

-- | Lens for 'ChainDifficulty' of 'MainBlockchain' in 'ConsensusData'.
mcdDifficulty :: Lens' (ConsensusData (MainBlockchain ssc)) ChainDifficulty
MAKE_LENS(mcdDifficulty, _mcdDifficulty)

-- | Lens for 'Signature' of 'MainBlockchain' in 'ConsensusData'.
mcdSignature :: Lens' (ConsensusData (MainBlockchain ssc)) (BlockSignature ssc)
MAKE_LENS(mcdSignature, _mcdSignature)

-- makeLensesData ''Body ''(MainBlockchain ssc)

-- | Lens for transaction payload in main block body.
mbTxPayload :: Lens' (Body (MainBlockchain ssc)) TxPayload
MAKE_LENS(mbTxPayload, _mbTxPayload)

-- | Lens for transaction tree in main block body.
mbTxs :: Lens' (Body (MainBlockchain ssc)) (MerkleTree Tx)
mbTxs = mbTxPayload . txpTxs

-- | Lens for witness list in main block body.
mbWitnesses :: Lens' (Body (MainBlockchain ssc)) [TxWitness]
mbWitnesses = mbTxPayload . txpWitnesses

-- | Lens for distributions list in main block body.
mbTxAddrDistributions :: Lens' (Body (MainBlockchain ssc)) [TxDistribution]
mbTxAddrDistributions = mbTxPayload . txpDistributions

-- | Lens for 'SscPayload' in main block body.
mbMpc :: Lens' (Body (MainBlockchain ssc)) (SscPayload ssc)
MAKE_LENS(mbMpc, _mbMpc)

-- | Lens for ProxySKs in main block body.
mbProxySKs :: Lens' (Body (MainBlockchain ssc)) [ProxySKHeavy]
MAKE_LENS(mbProxySKs, _mbProxySKs)

-- | Lens for 'UpdatePayload' in main block body.
mbUpdatePayload :: Lens' (Body (MainBlockchain ssc)) UpdatePayload
MAKE_LENS(mbUpdatePayload, _mbUpdatePayload)

makeLenses ''MainExtraHeaderData
makeLenses ''MainExtraBodyData

-- | Lens from 'MainBlockHeader' to 'PublicKey'.
headerLeaderKey :: Lens' (MainBlockHeader ssc) PublicKey
headerLeaderKey = gbhConsensus . mcdLeaderKey

-- | Lens from 'MainBlockHeader' to 'Signature'.
headerSignature :: Lens' (MainBlockHeader ssc) (BlockSignature ssc)
headerSignature = gbhConsensus . mcdSignature

-- | Lens from 'MainBlockHeader' to 'SlotId'.
headerSlot :: Lens' (MainBlockHeader ssc) SlotId
headerSlot = gbhConsensus . mcdSlot

-- | Lens from 'MainBlock' to 'SlotId'.
blockSlot :: BiHeader ssc => Lens' (MainBlock ssc) SlotId
blockSlot = gbHeader . headerSlot

-- | Lens from 'MainBlock' to 'PublicKey'.
blockLeaderKey :: Lens' (MainBlock ssc) PublicKey
blockLeaderKey = gbHeader . headerLeaderKey

-- | Lens from 'MainBlock' to 'Signature'.
blockSignature :: Lens' (MainBlock ssc) (BlockSignature ssc)
blockSignature = gbHeader . headerSignature

-- | Lens from 'MainBlock' to 'SscPayload'.
blockMpc :: Lens' (MainBlock ssc) (SscPayload ssc)
blockMpc = gbBody . mbMpc

-- | Lens from 'MainBlock' to 'MerkleTree'.
blockTxs :: Lens' (MainBlock ssc) (MerkleTree Tx)
blockTxs = gbBody . mbTxs

-- | Getter from 'MainBlock' to a list of transactions together with
-- auxiliary data.
blockTxas :: Getter (MainBlock ssc) [TxAux]
blockTxas =
    gbBody .
    to (\b -> zipWith3 TxAux (toList (b ^. mbTxs))
                             (b ^. mbWitnesses)
                             (b ^. mbTxAddrDistributions))

-- | Lens from 'MainBlock' to 'ProxySKHeavy' list.
blockProxySKs :: Lens' (MainBlock ssc) [ProxySKHeavy]
blockProxySKs = gbBody . mbProxySKs
