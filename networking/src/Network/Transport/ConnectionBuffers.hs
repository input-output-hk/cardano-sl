{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Transport.ConnectionBuffers (

      connectionBufferQDisc
    , Buffer(..)
    , BufferT
    , writeBuffer
    , readBuffer
    , readBuffer_
    , recvAtMost
    , ConnectionBuffersParams(..)
    , ConnectionEvent(..)
    , EndPointEvent(..)
    , EndPointBuffer
    , ConnectionBuffer
    , InternalError(..)

    ) where

import           Control.Monad (forM_, unless)
import           Control.Monad.IO.Class
import qualified Data.Binary.Get as Bin
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Network.Transport.Abstract
-- TODO use NonEmptySet instead.
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBQueue
import qualified Data.Set as S
import           Data.Typeable
import           Mockable.Class
import           Mockable.SharedAtomic

-- This connection-buffers QDisc needs an implementation of a bounded fifo
-- buffer such that the head can be inspected and modified.

type family BufferT (m :: * -> *) :: * -> *

data Buffer (m :: * -> *) (t :: *) where
    NewBuffer :: Int -> Buffer m (BufferT m t)
    -- | Read from the head of the buffer, with the option to replace the
    --   front with some other value. Useful in case you want to parse from
    --   a buffer and you have leftover data.
    ReadBuffer :: BufferT m t -> (t -> (Maybe t, r)) -> Buffer m r
    WriteBuffer :: BufferT m t -> t -> Buffer m ()
    -- | Impose a new bound on the buffer. No data should be dropped in case
    --   the new bound is lower than the old bound. That's to say, the buffer
    --   must maintain that writes are not possible when size >= bound, and
    --   size > bound is entirely possible.
    BoundBuffer :: BufferT m t -> (Int -> Int) -> Buffer m ()

instance
    ( BufferT m ~ BufferT n
    ) => MFunctor' Buffer m n
    where
    hoist' _ term = case term of
        NewBuffer i          -> NewBuffer i
        ReadBuffer buffer k  -> ReadBuffer buffer k
        WriteBuffer buffer t -> WriteBuffer buffer t
        BoundBuffer buffer b -> BoundBuffer buffer b

newBuffer :: ( Mockable Buffer m ) => Int -> m (BufferT m t)
newBuffer bound = liftMockable $ NewBuffer bound

writeBuffer :: ( Mockable Buffer m ) => BufferT m t -> t -> m ()
writeBuffer buffer t = liftMockable $ WriteBuffer buffer t

readBuffer :: ( Mockable Buffer m ) => BufferT m t -> (t -> (Maybe t, r)) -> m r
readBuffer buffer f = liftMockable $ ReadBuffer buffer f

readBuffer_ :: ( Mockable Buffer m ) => BufferT m t -> m t
readBuffer_ buffer = liftMockable $ ReadBuffer buffer (\t -> (Nothing, t))

boundBuffer :: ( Mockable Buffer m ) => BufferT m t -> (Int -> Int) -> m ()
boundBuffer buffer f = liftMockable $ BoundBuffer buffer f

-- | Try to decode a binary thing from a buffer. Any leftovers will be replaced.
--   Closed and Lost are always replaced.
decodeFromBuffer
    :: ( Mockable Buffer m )
    => Bin.Get t
    -> BufferT m ConnectionEvent
    -> m (Maybe t)
decodeFromBuffer get buffer = case Bin.runGetIncremental get of
    Bin.Partial continue -> go continue
    Bin.Done _ _ a       -> return (Just a)
    Bin.Fail _ _ _       -> return Nothing
    where

    -- Repeatedly read from the buffer and continue the parse.
    -- Leftover data is replaced when Done or Fail is encountered.
    go continue = do
        outcome <- readBuffer buffer $ \ev -> case ev of
            Closed -> (Just Closed, Nothing)
            Lost -> (Just Lost, Nothing)
            Data bs -> case continue (Just (BL.toStrict bs)) of
                Bin.Done trailing _ a -> (leftover trailing, Just (Right a))
                Bin.Fail trailing _ _ -> (leftover trailing, Nothing)
                Bin.Partial continue  -> (Nothing, Just (Left continue))
        case outcome of
            Nothing               -> return Nothing
            Just (Left continue') -> go continue'
            Just (Right t)        -> return (Just t)

    leftover bs = if BS.null bs then Nothing else Just (Data (BL.fromStrict bs))

data ConnectionEvent = Data BL.ByteString | Closed | Lost

data EndPointEvent m =
    PeerOpenedConnection !ConnectionId !Reliability !EndPointAddress !(ConnectionBuffer m)
  | LocalEndPointClosed
  | LocalEndPointFailed
  | LocalTransportFailed

type EndPointBuffer m = BufferT m (EndPointEvent m)

newEndPointBuffer :: ( Mockable Buffer m ) => Int -> m (EndPointBuffer m)
newEndPointBuffer = newBuffer

type ConnectionBuffer m = BufferT m ConnectionEvent

newConnectionBuffer :: ( Mockable Buffer m ) => Int -> m (ConnectionBuffer m)
newConnectionBuffer = newBuffer

-- | Receive at most n bytes from a connection buffer. If the head of the
--   buffer has more data than you asked for, the trailing data remains at
--   the head of the buffer. Otherwise, the first event is removed.
--   This will not block on a full queue, only on an empty one.
recvAtMost :: ( Mockable Buffer m ) => Int -> ConnectionBuffer m -> m ConnectionEvent
recvAtMost bytes cbuffer = readBuffer cbuffer $ \event -> case event of
    Lost -> (Nothing, Lost)
    Closed -> (Nothing, Closed)
    Data lbs -> do
        let (now, later) = BL.splitAt (fromIntegral bytes) lbs
        if BL.null later
        then (Nothing, Data now)
        else (Just (Data later), Data now)

-- | State that will be held internally by a connection-buffer QDisc and
--   updated when events are enqueued.
data ConnectionBuffersState m = ConnectionBuffersState {
    qdiscBuffers     :: !(M.Map ConnectionId (EndPointAddress, ConnectionBuffer m))
  , qdiscConnections :: !(M.Map EndPointAddress (S.Set ConnectionId))
  }

-- | Parameters for a connection-buffer QDisc.
data ConnectionBuffersParams m = ConnectionBuffersParams {
    qdiscEventBufferSize      :: Int
  , qdiscConnectionBufferSize :: Int
  , qdiscConnectionDataSize   :: Int
  , qdiscInternalError        :: forall t . InternalError -> m t
  }

-- | A QDisc with a fixed bound for the size of the event queue and also for
--   the size of the buffers for each connection.
connectionBufferQDisc
  :: forall m .
     ( Mockable SharedAtomic m, Mockable Buffer m )
  => ConnectionBuffersParams m
  -> m (QDisc m (EndPointEvent m))
connectionBufferQDisc qdiscParams = do

  let endPointBound = qdiscEventBufferSize qdiscParams
  let connectionBound = qdiscConnectionBufferSize qdiscParams
  let maxChunkSize = fromIntegral (qdiscConnectionDataSize qdiscParams)
  let internalError :: forall t . InternalError -> m t
      internalError = qdiscInternalError qdiscParams

  endPointBuffer <- newEndPointBuffer endPointBound
  let dequeue :: m (EndPointEvent m)
      dequeue = readBuffer_ endPointBuffer

  qdiscState :: SharedAtomicT m (ConnectionBuffersState m)
      <- newSharedAtomic (ConnectionBuffersState M.empty M.empty)

  let writeBufferRespectingChunkSize :: ConnectionBuffer m -> BL.ByteString -> m ()
      writeBufferRespectingChunkSize cbuffer lbs = do
        let (now, later) = BL.splitAt maxChunkSize lbs
        writeBuffer cbuffer (Data now)
        unless (BL.null later) (writeBufferRespectingChunkSize cbuffer later)

  let enqueue :: Event -> m ()
      enqueue event = case event of

        ConnectionOpened connid reliability addr -> do
          connectionBuffer :: ConnectionBuffer m <- newConnectionBuffer connectionBound
          () <- modifySharedAtomic qdiscState $ \st ->
            case M.lookup connid (qdiscBuffers st) of
              Nothing ->
                let alteration :: Maybe (S.Set ConnectionId) -> S.Set ConnectionId
                    alteration = maybe (S.singleton connid) (S.insert connid)
                    st' = st {
                        qdiscBuffers = M.insert connid (addr, connectionBuffer) (qdiscBuffers st)
                      , qdiscConnections = M.alter (Just . alteration) addr (qdiscConnections st)
                      }
                in  return (st', ())
              _ -> internalError DuplicateConnection
          let event = PeerOpenedConnection connid reliability addr connectionBuffer
          writeBuffer endPointBuffer event

        -- Events on a particular connection buffer will be consistent with
        -- their nt-tcp delivery because 'enqueue' will not be called
        -- concurrently for two events with the same 'ConnectionId'.

        ConnectionClosed connid -> do
          buffer <- modifySharedAtomic qdiscState $ \st ->
            case M.lookup connid (qdiscBuffers st) of
              Nothing -> internalError UnknownConnectionClosed
              Just (addr, buffer) ->
                let updateIt set = if S.null set' then Nothing else Just set'
                      where
                      set' = S.delete connid set
                    st' = st {
                        qdiscBuffers = M.delete connid (qdiscBuffers st)
                      , qdiscConnections = M.update updateIt addr (qdiscConnections st)
                      }
                in  return (st', buffer)
          writeBuffer buffer Closed

        Received connid bytes -> do
          buffer <- withSharedAtomic qdiscState $ \st ->
            case M.lookup connid (qdiscBuffers st) of
              Nothing          -> internalError UnknownConnectionReceived
              Just (_, buffer) -> return buffer
          -- Will block if the buffer is full, ultimately blocking the thread
          -- which is reading from the socket.
          writeBufferRespectingChunkSize buffer (BL.fromChunks bytes)

        EndPointClosed ->
          let event = LocalEndPointClosed
          in  writeBuffer endPointBuffer event

        ErrorEvent (TransportError (EventError EventEndPointFailed) _) ->
          let event = LocalEndPointFailed
          in  writeBuffer endPointBuffer event

        ErrorEvent (TransportError (EventError EventTransportFailed) _) ->
          let event = LocalTransportFailed
          in  writeBuffer endPointBuffer event

        -- Must find all connections for that address and write 'Lost' to their
        -- buffers.
        ErrorEvent (TransportError (EventError (EventConnectionLost addr)) _) -> do
          buffers <- modifySharedAtomic qdiscState $ \st ->
            case M.lookup addr (qdiscConnections st) of
              -- TBD: will network-transport report all of the connections to
              -- this peer as closed, before giving the connection lost
              -- message? I think so but I'm not sure.
              Nothing -> return (st, [])
              -- Get the buffers for all of the ConnectionIds and remove them
              -- from the state.
              -- connids is guaranteed non-empty set but GHC doesn't know that.
              Just connids ->
                let combine connid (buffers, deletedBuffers) =
                      case M.updateLookupWithKey (const (const Nothing)) connid buffers of
                        -- updateLookupWithKey returns the deleted value if it
                        -- was deleted.
                        (Just (_, buffer), buffers') -> (buffers', Just buffer : deletedBuffers)
                        _                            -> (buffers, Nothing : deletedBuffers)
                    (buffers, deletedBuffers) = foldr combine (qdiscBuffers st, []) (S.toList connids)
                    st' = st {
                        qdiscBuffers = buffers
                      , qdiscConnections = M.delete addr (qdiscConnections st)
                      }
                in  return (st', deletedBuffers)
          forM_ buffers $ maybe (internalError InconsistentConnectionState) (flip writeBuffer Lost)

  return $ QDisc enqueue dequeue

data InternalError =
    InconsistentConnectionState
  | UnknownConnectionClosed
  | UnknownConnectionLost
  | UnknownConnectionReceived
  | DuplicateConnection
  deriving (Typeable, Show)

instance Exception InternalError
