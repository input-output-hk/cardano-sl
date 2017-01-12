-- | Serialization of types defined in DB modules.

module Pos.Binary.DB
       (
       ) where

import           Data.Binary.Get     (getWord8)
import           Data.Binary.Put     (putWord8)
import           Universum

import           Pos.Binary.Class    (Bi (..))
import           Pos.DB.Types        (DecidedProposalState (..), GtRichmenStorage (..),
                                      LeadersStorage (..), ProposalState (..),
                                      StoredBlock (..), UndecidedProposalState (..))
import           Pos.Ssc.Class.Types (Ssc)

instance Ssc ssc =>
         Bi (StoredBlock ssc) where
    put StoredBlock {..} = put sbBlock >> put sbInMain
    get = StoredBlock <$> get <*> get

instance Bi (LeadersStorage ssc) where
    put LeadersStorage {..} = put lrcEpoch >> put lrcLeaders
    get = LeadersStorage <$> get <*> get

instance Bi (GtRichmenStorage ssc) where
    put GtRichmenStorage {..} = put gtRichmenEpoch >> put gtRichmen
    get = GtRichmenStorage <$> get <*> get

instance Bi UndecidedProposalState where
    put UndecidedProposalState {..} = do
        put upsVotes
        put upsProposal
        put upsSlot
        put upsPositiveStake
        put upsNegativeStake
    get = UndecidedProposalState <$> get <*> get <*> get <*> get <*> get

instance Bi DecidedProposalState where
    put DecidedProposalState {..} = do
        put dpsDecision
        put dpsProposal
        put dpsDifficulty
    get = DecidedProposalState <$> get <*> get <*> get

instance Bi ProposalState where
    put (PSUndecided us) = putWord8 0 >> put us
    put (PSDecided ds)   = putWord8 1 >> put ds
    get = getWord8 >>= \case
        0 -> PSUndecided <$> get
        1 -> PSDecided <$> get
