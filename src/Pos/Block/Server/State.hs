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
import           Data.List                     (last)
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as NE
import           Serokell.Util.Verify          (isVerSuccess)
import           Universum

import           Pos.Communication.Types.Block (MsgBlock (..), MsgGetHeaders (..))
import           Pos.Crypto                    (hash)
import           Pos.Ssc.Class.Types           (Ssc)
import           Pos.Types                     (Block, BlockHeader, HeaderHash,
                                                headerHash, prevBlockL, verifyHeaders)

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
    :: (Ssc ssc, MonadIO m, HasBlockSocketState s ssc)
    => NonEmpty (BlockHeader ssc) -> TVar s -> m Bool
matchRequestedHeaders (newTip :| hs) var =
    atomically $
    do blockSS <- readTVar var
       let res = maybe False
                       matchRequestedHeadersDo
                       (blockSS ^. bssRequestedHeaders)
       when res $ modifyTVar var $ bssRequestedHeaders .~ Nothing
       pure res
  where
    formChain = isVerSuccess (verifyHeaders True $ newTip:hs)
    matchRequestedHeadersDo mgh =
        let startHeader = bool (last hs) newTip $ null hs
            startMatches = hash startHeader `elem` mghFrom mgh
            mghToMatches = Just (hash newTip) == mghTo mgh
        in and [ startMatches
               , mghToMatches
               , formChain
               ]

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
    = PBmFinal (NonEmpty (Block ssc)) -- ^ Block is the last requested block.
    | PBmIntermediate                 -- ^ Block is expected one, but not last.
    | PBmUnsolicited                  -- ^ Block is not an expected one.

-- | Process 'Block' message received from peer.
processBlockMsg
    :: forall m s ssc.
       (Ssc ssc, MonadIO m, HasBlockSocketState s ssc)
    => MsgBlock ssc -> TVar s -> m (ProcessBlockMsgRes ssc)
processBlockMsg (MsgBlock blk) var =
    atomically $
    do st <- readTVar var
       case st ^. bssRequestedBlocks of
           Nothing    -> pure PBmUnsolicited
           Just range -> processBlockDo range (st ^. bssReceivedBlocks)
  where
    processBlockDo (start, end) []
        | headerHash blk == start = processBlockFinally end []
        | otherwise = pure PBmUnsolicited
    processBlockDo (start, end) received
        | blk ^. prevBlockL == start = processBlockFinally end received
        | otherwise = pure PBmUnsolicited
    processBlockFinally end received
        | headerHash blk == end =
            (PBmFinal $ NE.reverse $ blk :| received) <$ modifyTVar var invalidateBlocks
        | otherwise =
            PBmIntermediate <$ modifyTVar var (over bssReceivedBlocks (blk :))
    invalidateBlocks :: s -> s
    invalidateBlocks st =
        st & bssRequestedBlocks .~ Nothing & bssReceivedBlocks .~ []
