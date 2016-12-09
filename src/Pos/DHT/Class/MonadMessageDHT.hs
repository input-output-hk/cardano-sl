{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Pos.DHT.Class.MonadMessageDHT
       ( DHTMsgHeader (..)
       , MonadMessageDHT (..)
       , MonadResponseDHT (..)
       , DHTResponseT (..)
       , mapDHTResponseT
       , mapListenerDHT
       , WithDefaultMsgHeader (..)
       , ListenerDHT (..)
       , withDhtLogger
       , defaultSendToNeighbors
       , defaultSendToNode
       ) where

import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow, catch)
import           Control.Monad.Morph       (hoist)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Rpc      (BinaryP, Message, MonadDialog,
                                            MonadTransfer (..), NetworkAddress, ResponseT,
                                            closeR, hoistRespCond, mapResponseT, peerAddr,
                                            replyH, sendH)
import           Control.TimeWarp.Timed    (MonadTimed, ThreadId)
import           Data.Binary               (Binary)
import           Formatting                (int, sformat, shown, (%))
import qualified Formatting                as F
import           System.Wlog               (CanLog, HasLoggerName,
                                            WithLogger, logDebug, logInfo, logWarning)
import           Universum

import           Pos.Constants             (neighborsSendThreshold)
import           Pos.DHT.Types             (DHTNode (..), DHTNodeType (..), dhtAddr, filterByNodeType)
import           Pos.DHT.Class.MonadDHT
import           Pos.Util                  (messageName')


-- | Monad that can send messages over distributed network.
class MonadDHT m => MonadMessageDHT m where

    sendToNetwork :: (Binary r, Message r) => r -> m ()

    sendToNode :: (Binary r, Message r) => NetworkAddress -> r -> m ()

    sendToNeighbors :: (Binary r, Message r) => r -> m Int

    default sendToNode :: ( Binary r
                          , Message r
                          , WithLogger m
                          , WithDefaultMsgHeader m
                          , MonadDialog BinaryP m
                          , MonadThrow m
                          ) => NetworkAddress -> r -> m ()
    sendToNode = defaultSendToNode

    default sendToNeighbors :: ( Binary r
                               , Message r
                               , WithLogger m
                               , MonadCatch m
                               ) => r -> m Int
    sendToNeighbors = defaultSendToNeighbors sequence sendToNode

-- | Monad that can respond on messages for DHT algorithm.
class MonadMessageDHT m => MonadResponseDHT m where
  replyToNode :: (Binary r, Message r) => r -> m ()
  closeResponse :: m ()

instance MonadMessageDHT m => MonadMessageDHT (ReaderT r m) where
    sendToNetwork = lift . sendToNetwork
    sendToNeighbors = lift . sendToNeighbors
    sendToNode addr = lift . sendToNode addr

-- | Header of messages in DHT algorithm.
data DHTMsgHeader = BroadcastHeader
                  | SimpleHeader { dmhNoCache :: Bool }
  deriving (Generic, Show)

instance Binary DHTMsgHeader

-- | Class for something that has default 'DHTMsgHeader'.
class WithDefaultMsgHeader m where
    defaultMsgHeader :: Message r => r -> m DHTMsgHeader

instance (Monad m, WithDefaultMsgHeader m) => WithDefaultMsgHeader (ReaderT r m) where
    defaultMsgHeader = lift . defaultMsgHeader

instance MonadMessageDHT m => MonadMessageDHT (ResponseT m) where
    sendToNetwork = lift . sendToNetwork
    sendToNode node = lift . sendToNode node
    sendToNeighbors = lift . sendToNeighbors

instance (Monad m, WithDefaultMsgHeader m) => WithDefaultMsgHeader (ResponseT m) where
  defaultMsgHeader = lift . defaultMsgHeader

-- | Wrapper for monadic action that can also respond on DHT messages.
newtype DHTResponseT m a = DHTResponseT
    { getDHTResponseT :: ResponseT m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans,
                MonadThrow, MonadCatch, MonadMask,
                MonadState s, WithDefaultMsgHeader, CanLog,
                HasLoggerName, MonadTimed, MonadDialog t, MonadDHT, MonadMessageDHT)

instance MonadTransfer m => MonadTransfer (DHTResponseT m) where
    sendRaw addr p = DHTResponseT $ sendRaw addr (hoist getDHTResponseT p)
    listenRaw binding sink = DHTResponseT $ fmap DHTResponseT $ listenRaw binding (hoistRespCond getDHTResponseT sink)
    close = DHTResponseT . close

type instance ThreadId (DHTResponseT m) = ThreadId m

instance ( WithLogger m
         , WithDefaultMsgHeader m
         , MonadMessageDHT m
         , MonadDialog BinaryP m
         , MonadIO m
         , MonadMask m
         ) => MonadResponseDHT (DHTResponseT m) where
  replyToNode msg = do
    addr <- DHTResponseT $ peerAddr
    withDhtLogger $
      logDebug $ sformat ("Replying with message " % F.build % " to " % F.build) (messageName' msg) addr
    header <- defaultMsgHeader msg
    DHTResponseT $ replyH header msg
  closeResponse = DHTResponseT closeR

-- | Listener of DHT messages.
data ListenerDHT m =
    forall r . (Binary r, Message r)
            => ListenerDHT (r -> DHTResponseT m ())

-- | Monad morphism of inner monadic action inside 'DHTResponceT'.
mapDHTResponseT :: (m a -> n b) -> DHTResponseT m a -> DHTResponseT n b
mapDHTResponseT how = DHTResponseT . mapResponseT how . getDHTResponseT

-- | Helper for substituting inner monad stack in `ListenerDHT`
mapListenerDHT :: (m () -> n ()) -> ListenerDHT m -> ListenerDHT n
mapListenerDHT how (ListenerDHT listen) = ListenerDHT $ mapDHTResponseT how . listen

-- | Send 'defaultMsgHeader' for node with given 'NetworkAddress'.
defaultSendToNode
    :: ( MonadMessageDHT m
       , Binary r
       , Message r
       , WithDefaultMsgHeader m
       , WithLogger m
       , MonadDialog BinaryP m
       , MonadThrow m
       )
    => NetworkAddress -> r -> m ()
defaultSendToNode addr msg = do
    withDhtLogger $
      logDebug $ sformat ("Sending message " % F.build % " to node " % shown) (messageName' msg) addr
    header <- defaultMsgHeader msg
    sendH addr header msg

-- | Send default message to neighbours in parallel.
defaultSendToNeighbors
    :: ( MonadMessageDHT m
       , WithLogger m
       , MonadCatch m
       )
    => ([m Bool] -> m [Bool]) -> (NetworkAddress -> r -> m ()) -> r -> m Int
defaultSendToNeighbors parallelize sender msg = do
--    withDhtLogger $
--      logDebug $ sformat ("Sending message " % F.build % " neighbors") (messageName' msg)
    nodes <- filterByNodeType DHTFull <$> getKnownPeers
    succeed <- sendToNodes nodes
    succeed' <-
        if succeed < neighborsSendThreshold
            then (+) succeed <$>
                 do nodes' <- discoverPeers DHTFull
                    let newNodes = filter (flip notElem nodes) nodes'
                    sendToNodes newNodes
            else return succeed
    when (succeed' < neighborsSendThreshold) $
        logWarning $
        sformat
            ("Send to only " % int % " nodes, threshold is " % int)
            succeed'
            (neighborsSendThreshold :: Int)
    return succeed'
  where
    sendToNodes nodes = length . filter identity <$> parallelize (map send' nodes)
    send' node = (sender (dhtAddr node) msg >> return True) `catch` handleE
      where
        handleE (e :: SomeException) = do
          logInfo $ sformat ("Error sending message to " % F.build % ": " % shown) node e
          return False
