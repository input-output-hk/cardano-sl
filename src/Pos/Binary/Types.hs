{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Types.* modules

module Pos.Binary.Types where

import           Universum

import           Pos.Binary.Class    (Bi (..))
import           Pos.Binary.Merkle   ()
import           Pos.Ssc.Class.Types (Ssc (..))
import qualified Pos.Types.Timestamp as T
import qualified Pos.Types.Types     as T

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi T.Timestamp where
    get = fromInteger <$> get
    put = put . toInteger

instance Bi T.Coin where
    get = T.Coin <$> get
    put (T.Coin c) = put c

instance Bi T.EpochIndex where
    get = T.EpochIndex <$> get
    put (T.EpochIndex c) = put c

instance Bi T.LocalSlotIndex where
    get = T.LocalSlotIndex <$> get
    put (T.LocalSlotIndex c) = put c

instance Bi T.SlotId where
    put (T.SlotId e s) = put e >> put s
    get = T.SlotId <$> get <*> get

instance Bi T.TxIn where
    put (T.TxIn hash index) = put hash >> put index
    get = T.TxIn <$> get <*> get

instance Bi T.TxOut where
    put (T.TxOut addr coin) = put addr >> put coin
    get = T.TxOut <$> get <*> get

instance Bi T.Tx where
    put (T.Tx ins outs) = put ins >> put outs
    get = T.Tx <$> get <*> get

instance Bi T.TxInWitness where
    put (T.PkWitness key sig) = put key >> put sig
    get = T.PkWitness <$> get <*> get

-- serialized as vector of TxInWitness
--instance Bi T.TxWitness where

instance Bi T.SharedSeed where
    put (T.SharedSeed bs) = put bs
    get = T.SharedSeed <$> get

----------------------------------------------------------------------------
-- Generic block header
----------------------------------------------------------------------------

instance ( Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         ) =>
         Bi (T.GenericBlockHeader b) where
    put T.GenericBlockHeader{..} = do
        put _gbhPrevBlock
        put _gbhBodyProof
        put _gbhConsensus
        put _gbhExtra
    get = T.GenericBlockHeader <$> get <*> get <*> get <*> get

instance ( Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , Bi (T.Body b)
         , Bi (T.ExtraBodyData b)
         ) =>
         Bi (T.GenericBlock b) where
    put T.GenericBlock{..} = do
        put _gbHeader
        put _gbBody
        put _gbExtra
    get = T.GenericBlock <$> get <*> get <*> get

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

instance Bi T.ChainDifficulty where
    get = T.ChainDifficulty <$> get
    put (T.ChainDifficulty c) = put c

instance Ssc ssc => Bi (T.BodyProof (T.MainBlockchain ssc)) where
    put T.MainProof{..} = do
        put mpNumber
        put mpRoot
        put mpWitnessesHash
        put mpMpcProof
    get = T.MainProof <$> get <*> get <*> get <*> get

instance Ssc ssc => Bi (T.ConsensusData (T.MainBlockchain ssc)) where
    put T.MainConsensusData{..} = do
        put _mcdSlot
        put _mcdLeaderKey
        put _mcdDifficulty
        put _mcdSignature
    get = T.MainConsensusData <$> get <*> get <*> get <*> get

instance Ssc ssc => Bi (T.Body (T.MainBlockchain ssc)) where
    put T.MainBody{..} = do
        put _mbTxs
        put _mbWitnesses
        put _mbMpc
    get = T.MainBody <$> get <*> get <*> get

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

instance Bi (T.BodyProof (T.GenesisBlockchain ssc)) where
    put (T.GenesisProof h) = put h
    get = T.GenesisProof <$> get

instance Bi (T.ConsensusData (T.GenesisBlockchain ssc)) where
    put T.GenesisConsensusData{..} = put _gcdEpoch >> put _gcdDifficulty
    get = T.GenesisConsensusData <$> get <*> get

instance Bi (T.Body (T.GenesisBlockchain ssc)) where
    put (T.GenesisBody leaders) = put leaders
    get = T.GenesisBody <$> get
