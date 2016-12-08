{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | For Pos.Types.* modules

module Pos.Binary.Types where

import           Universum

import           Pos.Binary.Class    (Bi (..))
import           Pos.Ssc.Class.Types (Ssc (..))
import qualified Pos.Types.Timestamp as T
import qualified Pos.Types.Types     as T

-- TODO Write instances :p

instance Bi T.Timestamp where
  get = fromInteger <$> get
  put = put . toInteger

instance Bi T.Coin
instance Bi T.EpochIndex
instance Bi T.LocalSlotIndex
instance Bi T.SlotId
instance Bi T.TxIn
instance Bi T.TxOut
instance Bi T.Tx
instance Bi T.TxWitness
instance Bi T.SharedSeed

instance ( Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         ) =>
         Bi (T.GenericBlockHeader b)

instance ( Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , Bi (T.Body b)
         , Bi (T.ExtraBodyData b)
         ) =>
         Bi (T.GenericBlock b)

instance Bi T.ChainDifficulty
--instance Bi T.Validator
--instance Bi T.Redeemer

instance Ssc ssc => Bi (T.BodyProof (T.MainBlockchain ssc))
instance Ssc ssc => Bi (T.ConsensusData (T.MainBlockchain ssc))
instance Ssc ssc => Bi (T.Body (T.MainBlockchain ssc))

instance Bi (T.BodyProof (T.GenesisBlockchain ssc))
instance Bi (T.ConsensusData (T.GenesisBlockchain ssc))
instance Bi (T.Body (T.GenesisBlockchain ssc))

instance Bi T.TxInWitness
