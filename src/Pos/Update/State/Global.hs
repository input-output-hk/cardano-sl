{-# LANGUAGE TemplateHaskell #-}

-- | Global state of Update System. Contains relevant summary of data
-- stored in blocks. This state is stored in memory only. Another part
-- is stored in GStateDB, see Pos.DB.GState.

module Pos.Update.State.Global
       ( GlobalState (..)
       , HasGlobalState (globalState)
       ) where

import           Control.Lens (makeClassy)
import           Data.Default (Default (def))
import           Universum

import           Pos.Crypto   (PublicKey)
import           Pos.Types    (SoftwareVersion)

-- | This type represents summary of votes issued by stakeholder.
data VoteState
    = PositiveVote    -- ^ Stakeholder voted once positively.
    | NegativeVote    -- ^ Stakeholder voted once positively.
    | PositiveRevote  -- ^ Stakeholder voted negatively, then positively.
    | NegativeRevote  -- ^ Stakeholder voted positively, then negatively.

-- | In-memory global state of Update System.
data GlobalState = GlobalState
    { _gsProposals :: !(HashMap SoftwareVersion (HashMap PublicKey VoteState))
    }

-- | Classy lenses generated for GlobalState.
makeClassy ''GlobalState

instance Default GlobalState where
    def =
        GlobalState
        { _gsProposals = mempty
        }
