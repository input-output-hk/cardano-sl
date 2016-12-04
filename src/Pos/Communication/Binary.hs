{-# LANGUAGE StandaloneDeriving #-}

-- | Module containing binary instances for everything that's needed
-- to be passed on the wire.

module Pos.Communication.Binary where

import           Data.Binary         (Binary)
import           Data.Binary.Orphans ()

import qualified Pos.Types           as T

instance Binary T.Coin
instance Binary T.EpochIndex
instance Binary T.LocalSlotIndex
instance Binary T.SlotId
instance Binary T.TxIn
instance Binary T.TxOut
instance Binary T.Tx
instance Binary T.SharedSeed

instance ( Binary (BodyProof b)
         , Binary (ConsensusData b)
         , Binary (ExtraHeaderData b)
         ) =>
         Binary (GenericBlockHeader b)

instance ( Binary (BodyProof b)
         , Binary (ConsensusData b)
         , Binary (ExtraHeaderData b)
         , Binary (Body b)
         , Binary (ExtraBodyData b)
         ) =>
         Binary (GenericBlock b)

instance Binary T.ChainDifficulty
instance Binary T.Validator
instance Binary T.Redeemer

instance Ssc ssc => Binary (BodyProof (MainBlockchain ssc))
instance Ssc ssc => Binary (ConsensusData (MainBlockchain ssc))
instance Ssc ssc => Binary (Body (MainBlockchain ssc))

instance Binary (BodyProof (GenesisBlockchain ssc))
instance Binary (ConsensusData (GenesisBlockchain ssc))
instance Binary (Body (GenesisBlockchain ssc))
