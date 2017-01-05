-- | Serialization of types defined in DB modules.

module Pos.Binary.DB
       (
       ) where

import           Data.Binary         (Binary)
import           Universum

import           Pos.Binary.Class    (Bi (..))
import           Pos.DB.Types        (LrcStorage (..), ProposalState (..),
                                      StoredBlock (..), VoteState (..))
import           Pos.Ssc.Class.Types (Ssc)

instance Ssc ssc =>
         Bi (StoredBlock ssc) where
    put StoredBlock {..} = put sbBlock >> put sbInMain
    get = StoredBlock <$> get <*> get

instance Bi (LrcStorage ssc) where
    put LrcStorage {..} = put lrcEpoch >> put lrcLeaders >> put lrcRichmen
    get = LrcStorage <$> get <*> get <*> get

-- These types are used only for DB. But it still makes sense to
-- define serialization manually I suppose.
-- [CSL-124]
instance Binary VoteState
instance Bi VoteState

instance Bi ProposalState where
    put ProposalState {..} = put psVotes >> put psProposal >> put psSlot
    get = ProposalState <$> get <*> get <*> get
