-- | Server which handles blocks.

module Pos.Communication.Server.Block
       ( blockHandlers

       -- * Shouldn't be exported
       , getBlock
       , processNewBlock
       ) where

import           Universum

-- import           Pos.Crypto   (hash)
import qualified Pos.State    as St
import           Pos.Types    (Block, HeaderHash, MainBlockHeader)
import           Pos.WorkMode (WorkMode)

-- | Handlers for requests related to blocks processing.
-- TODO
blockHandlers :: [a]
blockHandlers = []

getBlock
    :: WorkMode m
    => HeaderHash -> m (Maybe Block)
getBlock = St.getBlock

processNewBlock :: WorkMode m => MainBlockHeader -> m ()
processNewBlock header = whenM (St.mayBlockBeUseful header) $ do
    -- blk <- undefined (hash $ Right header)  -- request block from node who sent it
    -- _ <- St.processNewBlocks [blk]
    pure ()
