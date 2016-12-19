{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Socket state of block processing server.

module Pos.Block.Server.State
       ( BlockSocketState
       , HasBlockSocketState (blockSocketState)
       , bssRequestedBlocks

       , recordHeadersRequest
       , recordBlocksRequest
       ) where

import           Control.Concurrent.STM        (TVar, modifyTVar, readTVar)
import           Control.Lens                  (makeClassy, set, view)
import           Data.Default                  (Default (def))
import           Universum

import           Pos.Communication.Types.Block (MsgGetBlocks, MsgGetHeaders)

-- | SocketState used for Block server.
data BlockSocketState ssc = BlockSocketState
    { -- | This field is filled when we request headers (sending
      -- `GetHeaders` message) and is invalidated when we receive
      -- corresponding 'Headers' message in response.
      _bssRequestedHeaders :: !(Maybe (MsgGetHeaders ssc))
    , -- | This field is filled when we request blocks (sending
      -- `GetBlocks` message) and is invalidated when we get all
      -- requested blocks.
      _bssRequestedBlocks  :: !(Maybe (MsgGetBlocks ssc))
    }

-- | Classy lenses generated for BlockSocketState.
makeClassy ''BlockSocketState

instance Default (BlockSocketState ssc) where
    def =
        BlockSocketState
        { _bssRequestedHeaders = Nothing
        , _bssRequestedBlocks = Nothing
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

-- | Record blocks request in BlockSocketState. This function blocks
-- if some blocks are requsted already.
recordBlocksRequest
    :: (MonadIO m, HasBlockSocketState s ssc)
    => MsgGetBlocks ssc -> TVar s -> m ()
recordBlocksRequest msg var =
    atomically $
    do existingMessage <- view bssRequestedBlocks <$> readTVar var
       case existingMessage of
           Nothing -> modifyTVar var (set bssRequestedBlocks (Just msg))
           Just _  -> retry
