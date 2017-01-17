-- | Logic of local data processing in Update System.

module Pos.Update.Logic.Local
       ( processProposal
       ) where

import           Universum

import           Pos.WorkMode (WorkMode)

processProposal :: WorkMode ssc m => m ()
processProposal = pass
