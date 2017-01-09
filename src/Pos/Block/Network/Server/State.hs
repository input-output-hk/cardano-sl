{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Socket state of block processing server.

module Pos.Block.Network.Server.State
       ( BlockPeerState
       , HasBlockPeerState (blockPeerState)
       , bssRequestedBlocks

       , recordHeadersRequest
       , matchRequestedHeaders
       , recordBlocksRequest

       , ProcessBlockMsgRes (..)
       , processBlockMsg
       ) where

import           Control.Lens            (makeClassy, over, set, view, (.~), (^.))
import           Data.Default            (Default (def))
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import qualified Data.List.NonEmpty      as NE
import           Mockable                (Mockable, SharedAtomic, SharedAtomicT,
                                          modifySharedAtomic, readSharedAtomic)
import           Serokell.Util.Verify    (isVerSuccess)
import           System.Wlog             (WithLogger, logInfo)
import           Universum

import           Pos.Block.Network.Types (MsgBlock (..), MsgGetHeaders (..))
import           Pos.Crypto              (hash)
import           Pos.Ssc.Class.Types     (Ssc)
import           Pos.Types               (Block, BlockHeader, HeaderHash, headerHash,
                                          prevBlockL, verifyHeaders)

-- | PeerState used for Block server.
data BlockPeerState ssc = BlockPeerState
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

-- | Classy lenses generated for BlockPeerState.
makeClassy ''BlockPeerState

instance Default (BlockPeerState ssc) where
    def =
        BlockPeerState
        { _bssRequestedHeaders = Nothing
        , _bssRequestedBlocks = Nothing
        , _bssReceivedBlocks = []
        }

-- | Record headers request in BlockPeerState. This function blocks
-- if some headers are requsted already.
recordHeadersRequest
    :: (HasBlockPeerState s ssc, Mockable SharedAtomic m, WithLogger m)
    => MsgGetHeaders ssc -> SharedAtomicT m s -> m Bool
recordHeadersRequest msg var = modifySharedAtomic var $ \st -> do
       case view bssRequestedHeaders st of
           Nothing -> return (set bssRequestedHeaders (Just msg) st, True)
           Just _  -> logInfo "Can't record headers request: another fetch in progress" $> (st, False)

-- | Try to match headers from 'Headers' message with requested
-- headers.  If 'bssRequestedHeaders' in socket state is Nothing or
-- `bssRequestedHeaders` doesn't match `Headers' message, nothing is
-- done and 'False' is returned.  If 'bssRequestedHeaders' in socket
-- state matches 'Headers' message, it's invalidated and 'True' is
-- returned.
matchRequestedHeaders
    :: (Ssc ssc, HasBlockPeerState s ssc, Mockable SharedAtomic m)
    => NonEmpty (BlockHeader ssc) -> SharedAtomicT m s -> m Bool
matchRequestedHeaders headers@(newTip :| hs) var = modifySharedAtomic var $ \st -> do
       let res = maybe False
                       matchRequestedHeadersDo
                       (st ^. bssRequestedHeaders)
       if res
          then pure ((bssRequestedHeaders .~ Nothing) st, res)
          else pure (st, res)
  where
    formChain = isVerSuccess (verifyHeaders True $ newTip:hs)
    matchRequestedHeadersDo mgh =
        let startHeader = NE.last headers
            startMatches = hash startHeader `elem` mghFrom mgh
            mghToMatches = Just (hash newTip) == mghTo mgh
        in and [ startMatches
               , mghToMatches
               , formChain
               ]

-- | Record blocks request in BlockPeerState. This function blocks
-- if some blocks are requsted already.
recordBlocksRequest
    :: (HasBlockPeerState s ssc, Mockable SharedAtomic m, WithLogger m)
    => HeaderHash ssc -> HeaderHash ssc -> SharedAtomicT m s -> m Bool
recordBlocksRequest fromHash toHash var = modifySharedAtomic var $ \st -> do
       case view bssRequestedBlocks st of
           Nothing -> return (set bssRequestedBlocks (Just (fromHash, toHash)) st, True)
           Just _  -> logInfo "Can't record blocks request: another fetch in progress" $> (st, False)

-- | Possible results of processBlockMsg.
data ProcessBlockMsgRes ssc
    = PBMfinal (NonEmpty (Block ssc)) -- ^ Block is the last requested block.
                                      --   Head is the oldest block here.
    | PBMintermediate                 -- ^ Block is expected one, but not last.
    | PBMunsolicited                  -- ^ Block is not an expected one.

-- | Process 'Block' message received from peer.
processBlockMsg
    :: forall m s ssc.
       (Ssc ssc, HasBlockPeerState s ssc, Mockable SharedAtomic m)
    => MsgBlock ssc -> SharedAtomicT m s -> m (ProcessBlockMsgRes ssc)
processBlockMsg (MsgBlock blk) var = modifySharedAtomic var $ \st -> do
       case st ^. bssRequestedBlocks of
           Nothing    -> pure (st, PBMunsolicited)
           Just range -> processBlockDo st range (st ^. bssReceivedBlocks)
  where
    processBlockDo st (start, end) []
        | headerHash blk == start = processBlockFinally st end []
        | otherwise = pure (st, PBMunsolicited)
    processBlockDo st (_, end) received@(lastReceived:_)
        | blk ^. prevBlockL == (headerHash lastReceived) =
            processBlockFinally st end received
        | otherwise = pure (st, PBMunsolicited)
    processBlockFinally st end received
        | headerHash blk == end =
            pure (invalidateBlocks st, PBMfinal $ NE.reverse $ blk :| received)
        | otherwise =
            pure (over bssReceivedBlocks (blk :) st, PBMintermediate)
    invalidateBlocks :: s -> s
    invalidateBlocks st =
        st & bssRequestedBlocks .~ Nothing & bssReceivedBlocks .~ []
