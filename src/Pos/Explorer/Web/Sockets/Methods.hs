{-# LANGUAGE TemplateHaskell #-}

-- | Logic of Explorer socket-io Server.

module Pos.Explorer.Web.Sockets.Methods
       ( ClientEvent (..)
       , ServerEvent (..)

       , startSession
       , finishSession
       , setClientAddress
       , setClientBlock
       , subscribeAddr
       , subscribeBlocks
       , unsubscribeAddr
       , unsubscribeBlocks
       , unsubscribeFully
       ) where

import           Control.Lens                    (at, (%=), (.=), _Just)
import           Control.Monad                   (join)
import           Control.Monad.State             (MonadState)
import qualified Data.Set                        as S
import           Network.EngineIO                (SocketId)
import           Network.SocketIO                (Socket, socketId)
import           Universum

import           Pos.Explorer.Web.Sockets.Holder (ConnectionsState, ccAddress, ccBlock,
                                                  csAddressSubscribers,
                                                  csBlocksSubscribers, csClients,
                                                  mkClientContext)
import           Pos.Explorer.Web.Sockets.Util   (EventName (..))
import           Pos.Types                       (Address, ChainDifficulty)

data ClientEvent
    = StartSession
    | SubscribeAddr
    | SubscribeBlock
    | UnsubscribeAddr
    | UnsubscribeBlock
    | SetClientAddress
    | SetClientBlock

instance EventName ClientEvent where
    toName StartSession     = "S"
    toName SubscribeAddr    = "SA"
    toName SubscribeBlock   = "SB"
    toName UnsubscribeAddr  = "UA"
    toName UnsubscribeBlock = "UB"
    toName SetClientAddress = "CA"
    toName SetClientBlock   = "CB"

data ServerEvent
    = AddrUpdated
    | BlocksUpdated

startSession
    :: MonadState ConnectionsState m
    => Socket -> m ()
startSession conn = do
    let cc = mkClientContext conn
        id = socketId conn
    csClients . at id .= Just cc

finishSession :: MonadState ConnectionsState m => SocketId -> m ()
finishSession i = whenJustM (use $ csClients . at i) finishSessionDo
  where
    finishSessionDo _ = do
        csClients . at i .= Nothing
        unsubscribeBlocks i
        unsubscribeAddr i

setClientAddress
    :: MonadState ConnectionsState m
    => SocketId -> Maybe Address -> m ()
setClientAddress sessId addr = do
    unsubscribeAddr sessId
    csClients . at sessId . _Just . ccAddress .= addr
    whenJust addr $ subscribeAddr sessId

setClientBlock
    :: MonadState ConnectionsState m
    => SocketId -> Maybe ChainDifficulty -> m ()
setClientBlock sessId pId = do
    csClients . at sessId . _Just . ccBlock .= pId
    subscribeBlocks sessId

subscribeAddr
    :: MonadState ConnectionsState m
    => SocketId -> Address -> m ()
subscribeAddr i addr =
    csAddressSubscribers . at addr %= Just .
    (maybe (S.singleton i) (S.insert i))

unsubscribeAddr
    :: MonadState ConnectionsState m
    => SocketId -> m ()
unsubscribeAddr i = do
    addr <- preuse $ csClients . at i . _Just . ccAddress
    whenJust (join addr) unsubscribeDo
  where
    unsubscribeDo a = csAddressSubscribers . at a %= fmap (S.delete i)

subscribeBlocks
    :: MonadState ConnectionsState m
    => SocketId -> m ()
subscribeBlocks i = csBlocksSubscribers %= S.insert i

unsubscribeBlocks
    :: MonadState ConnectionsState m
    => SocketId -> m ()
unsubscribeBlocks i = csBlocksSubscribers %= S.delete i

unsubscribeFully
    :: MonadState ConnectionsState m
    => SocketId -> m ()
unsubscribeFully i = unsubscribeBlocks i >> unsubscribeAddr i
