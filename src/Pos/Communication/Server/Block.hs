-- | Server which handles blocks.

module Pos.Communication.Server.Block
       ( blockHandlers

       -- * Shouldn't be exported
       , getBlock
       ) where

import           Universum

import qualified Pos.State    as St
import           Pos.Types    (Block, HeaderHash)
import           Pos.WorkMode (WorkMode)

-- | Handlers for requests related to blocks processing.
-- TODO
blockHandlers :: [a]
blockHandlers = []

getBlock
    :: WorkMode m
    => St.NodeState -> HeaderHash -> m (Maybe Block)
getBlock st = St.getBlock st
