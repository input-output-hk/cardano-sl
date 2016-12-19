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
       , matchRequestedHeaders
       , recordBlocksRequest
       ) where

import           Control.Concurrent.STM        (TVar, modifyTVar, readTVar)
import           Control.Lens                  (makeClassy, set, view)
import           Data.Default                  (Default (def))
import           Data.List.NonEmpty            (NonEmpty)
import           Universum

import           Pos.Communication.Types.Block (MsgGetHeaders)
import           Pos.Types                     (BlockHeader, HeaderHash)

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
    do existingMessage <- view bssRequestedBlocks <$> readTVar var
       case existingMessage of
           Nothing ->
               modifyTVar var (set bssRequestedBlocks (Just (fromHash, toHash)))
           Just _ -> retry
