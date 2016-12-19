{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Socket state of block processing server.

module Pos.Block.Server.State
       ( BlockSocketState
       , HasBlockSocketState (blockSocketState)
       , bssRequestedBlocks

       , recordBlocksRequest
       ) where

import           Control.Concurrent.STM        (TVar, modifyTVar, readTVar)
import           Control.Lens                  (makeClassy, set, view)
import           Data.Default                  (Default (def))
import           Universum

import           Pos.Communication.Types.Block (MsgGetBlocks)

-- | SocketState used for Block server.
data BlockSocketState ssc = BlockSocketState
    { -- | This field is filled when we request blocks (sending
      -- `GetBlocks` message) and is invalidated when we get all
      -- requested blocks.
      _bssRequestedBlocks :: !(Maybe (MsgGetBlocks ssc))
    }

-- | Classy lenses generated for BlockSocketState.
makeClassy ''BlockSocketState

instance Default (BlockSocketState ssc) where
    def =
        BlockSocketState
        { _bssRequestedBlocks = Nothing
        }

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
