{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Distributed Hash Table for peer discovery.

module Pos.DHT (
    DHTException (..),
    DHTKey,
    dhtKeyBytes,
    DHTData,
    DHTNode (..),
    DHTNodeType (..),
    DHTMsgHeader (..),
    MonadDHT (..),
    MonadMessageDHT (..),
    MonadResponseDHT (..),
    DHTResponseT (..),
    mapDHTResponseT,
    mapListenerDHT,
    randomDHTKey,
    bytesToDHTKey,
    dhtNodeType,
    WithDefaultMsgHeader (..),
    ListenerDHT (..),
    withDhtLogger,
    filterByNodeType,
    joinNetworkNoThrow,
    defaultSendToNeighbors,
    defaultSendToNode
) where

import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow, catch,
                                            throwM)
import           Control.Monad.Morph       (hoist)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Rpc      (BinaryP, HeaderNContentData, Message,
                                            MonadDialog, MonadTransfer (..),
                                            NetworkAddress, ResponseT, Unpackable, closeR,
                                            hoistRespCond, mapResponseT, peerAddr, replyH,
                                            sendH)
import           Control.TimeWarp.Timed    (MonadTimed, ThreadId)
import           Data.Binary               (Binary)
import           Data.Proxy                (Proxy (Proxy))
import           Formatting                (int, sformat, shown, (%))
import qualified Formatting                as F
import           System.Wlog               (LoggerName,
                                            WithNamedLogger (modifyLoggerName), logDebug,
                                            logInfo, logWarning)
import           Universum

import           Pos.Constants             (neighborsSendThreshold)
import           Pos.DHT.Types
import           Pos.Util                  (messageName')

class Monad m => MonadDHT m where
    joinNetwork :: [DHTNode] -> m ()

  -- Peer discovery: query DHT for random key
  -- Processing request, node will discover few other nodes
  -- We return these newly discovered nodes among with already known
  -- (List of known nodes is updated as well)
    discoverPeers :: DHTNodeType -> m [DHTNode]

    getKnownPeers :: m [DHTNode]

    currentNodeKey :: m DHTKey

    dhtLoggerName :: Proxy m -> LoggerName
    -- dhtLoggerName Proxy = "MonadDHT"

dhtLoggerNameM :: forall m . MonadDHT m => m LoggerName
dhtLoggerNameM = pure $ dhtLoggerName (Proxy :: Proxy m)

withDhtLogger
    :: (WithNamedLogger m, MonadDHT m)
    => m a -> m a
withDhtLogger action = do
    subName <- dhtLoggerNameM
    modifyLoggerName (<> subName) action

data DHTMsgHeader = BroadcastHeader
                  | SimpleHeader { dmhNoCache :: Bool }
  deriving (Generic, Show)

instance Binary DHTMsgHeader

class WithDefaultMsgHeader m where
    defaultMsgHeader :: Message r => r -> m DHTMsgHeader

class MonadDHT m => MonadMessageDHT m where

    sendToNetwork :: (Binary r, Message r) => r -> m ()

    sendToNode :: (Binary r, Message r) => NetworkAddress -> r -> m ()

    sendToNeighbors :: (Binary r, Message r) => r -> m Int

    default sendToNode :: ( Binary r
                          , Message r
                          , WithNamedLogger m
                          , WithDefaultMsgHeader m
                          , MonadIO m
                          , MonadDialog BinaryP m
                          , MonadThrow m
                          ) => NetworkAddress -> r -> m ()
    sendToNode = defaultSendToNode

    default sendToNeighbors :: (Binary r, Message r, WithNamedLogger m, MonadCatch m, MonadIO m) => r -> m Int
    sendToNeighbors = defaultSendToNeighbors sequence sendToNode

class MonadMessageDHT m => MonadResponseDHT m where
  replyToNode :: (Binary r, Message r) => r -> m ()
  closeResponse :: m ()

data ListenerDHT m =
    forall r . (Unpackable BinaryP (HeaderNContentData DHTMsgHeader r), Message r)
            => ListenerDHT (r -> DHTResponseT m ())

defaultSendToNode
    :: ( MonadMessageDHT m
       , Binary r
       , Message r
       , WithDefaultMsgHeader m
       , WithNamedLogger m
       , MonadDialog BinaryP m
       , MonadThrow m
       , MonadIO m
       )
    => NetworkAddress -> r -> m ()
defaultSendToNode addr msg = do
    withDhtLogger $
      logDebug $ sformat ("Sending message " % F.build % " to node " % shown) (messageName' msg) addr
    header <- defaultMsgHeader msg
    sendH addr header msg

defaultSendToNeighbors
    :: ( MonadMessageDHT m
       , WithNamedLogger m
       , MonadCatch m
       , MonadIO m
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

data DHTException = NodeDown | AllPeersUnavailable
  deriving (Show, Typeable)

instance Exception DHTException

instance MonadDHT m => MonadDHT (ResponseT m) where
    discoverPeers = lift . discoverPeers
    getKnownPeers = lift getKnownPeers
    currentNodeKey = lift currentNodeKey
    joinNetwork = lift . joinNetwork
    dhtLoggerName _ = dhtLoggerName (Proxy :: Proxy m)

instance MonadMessageDHT m => MonadMessageDHT (ResponseT m) where
    sendToNetwork = lift . sendToNetwork
    sendToNode node = lift . sendToNode node
    sendToNeighbors = lift . sendToNeighbors

instance (Monad m, WithDefaultMsgHeader m) => WithDefaultMsgHeader (ResponseT m) where
  defaultMsgHeader = lift . defaultMsgHeader

newtype DHTResponseT m a = DHTResponseT
    { getDHTResponseT :: ResponseT m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans,
                MonadThrow, MonadCatch, MonadMask,
                MonadState s, WithDefaultMsgHeader,
                WithNamedLogger, MonadTimed, MonadDialog t, MonadDHT, MonadMessageDHT)

instance MonadTransfer m => MonadTransfer (DHTResponseT m) where
    sendRaw addr p = DHTResponseT $ sendRaw addr (hoist getDHTResponseT p)
    listenRaw binding sink = DHTResponseT $ fmap DHTResponseT $ listenRaw binding (hoistRespCond getDHTResponseT sink)
    close = DHTResponseT . close

type instance ThreadId (DHTResponseT m) = ThreadId m

instance (WithNamedLogger m, WithDefaultMsgHeader m, MonadMessageDHT m, MonadDialog BinaryP m, MonadIO m, MonadMask m) => MonadResponseDHT (DHTResponseT m) where
  replyToNode msg = do
    addr <- DHTResponseT $ peerAddr
    withDhtLogger $
      logDebug $ sformat ("Replying with message " % F.build % " to " % F.build) (messageName' msg) addr
    header <- defaultMsgHeader msg
    DHTResponseT $ replyH header msg
  closeResponse = DHTResponseT closeR

mapDHTResponseT :: (m a -> n b) -> DHTResponseT m a -> DHTResponseT n b
mapDHTResponseT how = DHTResponseT . mapResponseT how . getDHTResponseT

-- | Helper for substituting inner monad stack in `ListenerDHT`
mapListenerDHT :: (m () -> n ()) -> ListenerDHT m -> ListenerDHT n
mapListenerDHT how (ListenerDHT listen) = ListenerDHT $ mapDHTResponseT how . listen

filterByNodeType :: DHTNodeType -> [DHTNode] -> [DHTNode]
filterByNodeType type_ = filter (\n -> dhtNodeType (dhtNodeId n) == Just type_)

joinNetworkNoThrow :: (MonadDHT m, MonadCatch m, MonadIO m, WithNamedLogger m) => [DHTNode] -> m ()
joinNetworkNoThrow peers = joinNetwork peers `catch` handleJoinE
  where
    handleJoinE AllPeersUnavailable =
        logInfo $ sformat ("Not connected to any of peers " % F.build) peers
    handleJoinE e = throwM e
