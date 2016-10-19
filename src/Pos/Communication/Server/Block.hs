-- | Server which handles blocks.

module Pos.Communication.Server.Block
       ( blockListeners

       , handleBlockHeader -- tmp
       ) where

import           Control.TimeWarp.Rpc    (Listener (..), MonadDialog, MonadResponse,
                                          reply)
import           Universum

import           Pos.Communication.Types (RequestBlock (..), SendBlock (..),
                                          SendBlockHeader (..))
import           Pos.Crypto              (hash)
import qualified Pos.State               as St
import           Pos.WorkMode            (WorkMode)

-- | Listeners for requests related to blocks processing.
blockListeners :: WorkMode m => [Listener m]
blockListeners =
    [ Listener handleBlock
    -- , Listener handleBlockHeader
    , Listener handleBlockRequest
    ]

handleBlock :: WorkMode m => SendBlock -> m ()
handleBlock (SendBlock block) = do
    _ <- St.processBlock block
    notImplemented

handleBlockHeader
    :: (WorkMode m, MonadResponse m, MonadDialog m)  -- TODO: MonadDialog should be part of WorkMode
    => SendBlockHeader -> m ()
handleBlockHeader (SendBlockHeader header) = whenM (St.mayBlockBeUseful header) $ do
    let h = hash $ Right header
    reply (RequestBlock h)

handleBlockRequest
    :: WorkMode m
    => RequestBlock -> m ()
handleBlockRequest (RequestBlock h) = do
    _ <- St.getBlock h
    -- reply (SendBlock blk)
    notImplemented
