{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Socket state of block processing server.

module Pos.Block.Server.State
       ( BlockSocketState
       , HasBlockSocketState (blockSocketState)
       , bssRequestedBlocks

       , recordHeadersRequest
       , matchRequestedHeaders
       , recordBlocksRequest

       , ProcessBlockMsgRes (..)
       , processBlockMsg
       ) where

import           Control.Concurrent.STM        (TVar, modifyTVar, readTVar)
import           Control.Lens                  (makeClassy, over, set, view, (.~), (^.))
import           Data.Default                  (Default (def))
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as NE
import           Universum

import           Pos.Communication.Types.Block (MsgBlock (..), MsgGetHeaders)
import           Pos.Ssc.Class.Types           (Ssc)
import           Pos.Types                     (Block, BlockHeader, HeaderHash,
                                                headerHash, prevBlockL)

-- | SocketState used for Block server.
data BlockSocketState ssc = BlockSocketState
    { -- | This field is filled when we request headers (sending
      -- `GetHeaders` message) and is invalidated when we receive
      -- corresponding 'Headers' message in response.
      _bssRequestedHeaders :: !(Maybe (MsgGetHeaders ssc))
    , -- | This field is filled when we request blocks (sending
      -- `GetBlocks` message) and is invalidated when we get all
      -- requested blocks.
      _bssRequestedBlocks  :: !(Maybe (HeaderHash ssc, HeaderHash ssc))
    , -- | Received blocks are accumulated to be processed later.
      -- IMPORTANT: head is the last received block.
      _bssReceivedBlocks   :: ![Block ssc]
    }

-- | Classy lenses generated for BlockSocketState.
makeClassy ''BlockSocketState

instance Default (BlockSocketState ssc) where
    def =
        BlockSocketState
        { _bssRequestedHeaders = Nothing
        , _bssRequestedBlocks = Nothing
        , _bssReceivedBlocks = []
        }

-- | Record headers request in BlockSocketState. This function blocks
-- if some headers are requsted already.
recordHeadersRequest
    :: (MonadIO m, HasBlockSocketState s ssc)
    => MsgGetHeaders ssc -> TVar s -> m ()
recordHeadersRequest msg var =
    atomically $
    do existingMessage <- view bssRequestedHeaders <$> readTVar var
       case existingMessage of
           Nothing -> modifyTVar var (set bssRequestedHeaders (Just msg))
           Just _  -> retry

-- | Try to match headers from 'Headers' message with requested
-- headers.  If 'bssRequestedHeaders' in socket state is Nothing or
-- `bssRequestedHeaders` doesn't match `Headers' message, nothing is
-- done and 'False' is returned.  If 'bssRequestedHeaders' in socket
-- state matches 'Headers' message, it's invalidated and 'True' is
-- returned.
matchRequestedHeaders
    :: (MonadIO m, HasBlockSocketState s ssc)
    => NonEmpty (BlockHeader ssc) -> TVar s -> m Bool
matchRequestedHeaders = notImplemented

-- | Record blocks request in BlockSocketState. This function blocks
-- if some blocks are requsted already.
recordBlocksRequest
    :: (MonadIO m, HasBlockSocketState s ssc)
    => HeaderHash ssc -> HeaderHash ssc -> TVar s -> m ()
recordBlocksRequest fromHash toHash var =
    atomically $
    do existing <- view bssRequestedBlocks <$> readTVar var
       case existing of
           Nothing ->
               modifyTVar var (set bssRequestedBlocks (Just (fromHash, toHash)))
           Just _ -> retry

-- | Possible results of processBlockMsg.
data ProcessBlockMsgRes ssc
    = PBMfinal (NonEmpty (Block ssc)) -- ^ Block is the last requested block.
    | PBMintermediate                 -- ^ Block is expected one, but not last.
    | PBMunsolicited                  -- ^ Block is not an expected one.

-- | Process 'Block' message received from peer.
processBlockMsg
    :: forall m s ssc.
       (Ssc ssc, MonadIO m, HasBlockSocketState s ssc)
    => MsgBlock ssc -> TVar s -> m (ProcessBlockMsgRes ssc)
processBlockMsg (MsgBlock blk) var =
    atomically $
    do st <- readTVar var
       case st ^. bssRequestedBlocks of
           Nothing    -> pure PBMunsolicited
           Just range -> processBlockDo range (st ^. bssReceivedBlocks)
  where
    processBlockDo (start, end) []
        | headerHash blk == start = processBlockFinally end []
        | otherwise = pure PBMunsolicited
    processBlockDo (start, end) received
        | blk ^. prevBlockL == start = processBlockFinally end received
        | otherwise = pure PBMunsolicited
    processBlockFinally end received
        | headerHash blk == end =
            (PBMfinal $ NE.reverse $ blk :| received) <$ modifyTVar var invalidateBlocks
        | otherwise =
            PBMintermediate <$ modifyTVar var (over bssReceivedBlocks (blk :))
    invalidateBlocks :: s -> s
    invalidateBlocks st =
        st & bssRequestedBlocks .~ Nothing & bssReceivedBlocks .~ []
