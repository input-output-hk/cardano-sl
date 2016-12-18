{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Pos.DHT.Model.Class.MonadMessageDHT
       (
         DHTMsgHeader (..)
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
       , DHTPacking
       , MonadDHTDialog
       , DHTDialog
       ) where

import           Control.Monad.Catch          (MonadCatch, MonadMask, MonadThrow, catch)
import           Control.Monad.Morph          (hoist)
import           Control.Monad.Trans.Class    (MonadTrans)
import           Control.TimeWarp.Rpc         (Dialog, Message, MonadDialog,
                                               MonadTransfer (..), NetworkAddress,
                                               ResponseT, closeR, hoistRespCond,
                                               mapResponseT, peerAddr, replyH, sendH)
import           Control.TimeWarp.Timed       (MonadTimed, ThreadId)
import           Formatting                   (int, sformat, shown, (%))
import qualified Formatting                   as F
import           System.Wlog                  (CanLog, HasLoggerName, WithLogger,
                                               logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Class             (Bi (..))
import           Pos.Constants                (neighborsSendThreshold)
import           Pos.DHT.Model.Class.BiP      (BiP)
import           Pos.DHT.Model.Class.MonadDHT
import           Pos.DHT.Model.Types          (DHTNode (..), DHTNodeType (..), dhtAddr,
                                               filterByNodeType)
import           Pos.Util                     (messageName')

-- | Monad that can send messages over distributed network.
class MonadDHT m => MonadMessageDHT s m | m -> s where

    sendToNetwork :: (Bi r, Message r) => r -> m ()

    sendToNode :: (Bi r, Message r) => NetworkAddress -> r -> m ()

    sendToNeighbors :: (Bi r, Message r) => r -> m Int

    default sendToNode :: ( Bi r
                          , Bi DHTMsgHeader
                          , Message r
                          , WithLogger m
                          , WithDefaultMsgHeader m
                          , MonadDHTDialog s m
                          , MonadThrow m
                          ) => NetworkAddress -> r -> m ()
    sendToNode = defaultSendToNode

    default sendToNeighbors :: ( Bi r
                               , Message r
                               , WithLogger m
                               , MonadCatch m
                               ) => r -> m Int
    sendToNeighbors = defaultSendToNeighbors sequence sendToNode

-- | Monad that can respond on messages for DHT algorithm.
class MonadMessageDHT s m => MonadResponseDHT s m | m -> s where
  replyToNode :: (Bi r, Message r) => r -> m ()
  closeResponse :: m ()

instance MonadMessageDHT s m => MonadMessageDHT s (ReaderT r m) where
    sendToNetwork = lift . sendToNetwork
    sendToNeighbors = lift . sendToNeighbors
    sendToNode addr = lift . sendToNode addr

-- | Header of messages in DHT algorithm.
data DHTMsgHeader = BroadcastHeader
                  | SimpleHeader { dmhNoCache :: Bool }
  deriving (Generic, Show)

-- | Class for something that has default 'DHTMsgHeader'.
class WithDefaultMsgHeader m where
    defaultMsgHeader :: Message r => r -> m DHTMsgHeader

instance (Monad m, WithDefaultMsgHeader m) => WithDefaultMsgHeader (ReaderT r m) where
    defaultMsgHeader = lift . defaultMsgHeader

instance MonadMessageDHT s m => MonadMessageDHT s (ResponseT s m) where
    sendToNetwork = lift . sendToNetwork
    sendToNode node = lift . sendToNode node
    sendToNeighbors = lift . sendToNeighbors

instance (Monad m, WithDefaultMsgHeader m) => WithDefaultMsgHeader (ResponseT s m) where
  defaultMsgHeader = lift . defaultMsgHeader

  -- | Packing type used by DHT to send messages.
type DHTPacking = BiP DHTMsgHeader

-- | Shortcut for `MonadDialog` with packing used by DHT.
type MonadDHTDialog s = MonadDialog s DHTPacking

-- | Shortcut for `Dialog` with packing used by DHT.
type DHTDialog = Dialog DHTPacking

-- | Wrapper for monadic action that can also respond on DHT messages.
newtype DHTResponseT s m a = DHTResponseT
    { getDHTResponseT :: ResponseT s m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans,
                MonadThrow, MonadCatch, MonadMask,
                MonadState ss, WithDefaultMsgHeader, CanLog,
                HasLoggerName, MonadTimed, MonadDialog s p, MonadDHT, MonadMessageDHT s
                )

instance MonadTransfer s m => MonadTransfer s (DHTResponseT s m) where
    sendRaw addr p = DHTResponseT $ sendRaw addr (hoist getDHTResponseT p)
    listenRaw binding sink =
        DHTResponseT $ fmap DHTResponseT $ listenRaw binding (hoistRespCond getDHTResponseT sink)
    close = DHTResponseT . close
    userState = DHTResponseT . userState

type instance ThreadId (DHTResponseT s m) = ThreadId m

instance ( WithLogger m
         , WithDefaultMsgHeader m
         , MonadMessageDHT s m
         , MonadDHTDialog s m
         , MonadIO m
         , MonadMask m
         , Bi DHTMsgHeader
         ) => MonadResponseDHT s (DHTResponseT s m) where
  replyToNode msg = do
    addr <- DHTResponseT $ peerAddr
    withDhtLogger $
      logDebug $
      sformat ("Replying with message "%F.build%" to "%F.build) (messageName' msg) addr
    header <- defaultMsgHeader msg
    DHTResponseT $ replyH header msg
  closeResponse = DHTResponseT closeR

-- | Listener of DHT messages.
data ListenerDHT s m =
    forall r . (Bi r, Message r)
            => ListenerDHT (r -> DHTResponseT s m ())

-- | Monad morphism of inner monadic action inside 'DHTResponceT'.
mapDHTResponseT :: (m a -> n b) -> DHTResponseT s m a -> DHTResponseT s n b
mapDHTResponseT how = DHTResponseT . mapResponseT how . getDHTResponseT

-- | Helper for substituting inner monad stack in `ListenerDHT`
mapListenerDHT :: (m () -> n ()) -> ListenerDHT s m -> ListenerDHT s n
mapListenerDHT how (ListenerDHT listen) = ListenerDHT $ mapDHTResponseT how . listen

-- | Send 'defaultMsgHeader' for node with given 'NetworkAddress'.
defaultSendToNode
    :: ( MonadMessageDHT s m
       , Bi r
       , Message r
       , WithDefaultMsgHeader m
       , WithLogger m
       , MonadDHTDialog s m
       , MonadThrow m
       , Bi DHTMsgHeader
       )
    => NetworkAddress -> r -> m ()
defaultSendToNode addr msg = do
    withDhtLogger $
      logDebug $
      sformat ("Sending message "%F.build%" to node "%shown) (messageName' msg) addr
    header <- defaultMsgHeader msg
    sendH addr header msg

-- | Send default message to neighbours in parallel.
defaultSendToNeighbors
    :: ( MonadMessageDHT s m
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
