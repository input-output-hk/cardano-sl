{-# LANGUAGE TemplateHaskell #-}

-- | Logic of Explorer socket-io Server.

module Pos.Explorer.Web.Sockets.Methods
       ( startSession
       , finishSession
       , setClientAddress
       , setClientBlock
       , subscribeAddr
       , subscribeBlocks
       , unsubscribeAddr
       , unsubscribeBlocks
       , unsubscribeFully
       ) where

import           Control.Lens                    (at, (%=), (+=), (.=), _Just)
import           Control.Monad                   (join)
import           Control.Monad.State             (MonadState)
import qualified Data.Set                        as S
import           Network.SocketIO                (Socket)
import           Universum

import           Pos.Explorer.Web.Sockets.Holder (ConnectionsState, SessionId, ccAddress,
                                                  ccBlock, csAddressSubscribers,
                                                  csBlocksSubscribers, csClients,
                                                  csCounter, mkClientContext)
import           Pos.Types                       (Address, ChainDifficulty)

startSession
    :: MonadState ConnectionsState m
    => Socket -> m SessionId
startSession conn = do
    i <- use csCounter
    csCounter += 1
    let cc = mkClientContext conn
    i <$ (csClients . at i .= Just cc)

finishSession :: MonadState ConnectionsState m => SessionId -> m ()
finishSession i = whenJustM (use $ csClients . at i) finishSessionDo
  where
    finishSessionDo _ = do
        csClients . at i .= Nothing
        unsubscribeBlocks i
        unsubscribeAddr i

setClientAddress
    :: MonadState ConnectionsState m
    => SessionId -> Maybe Address -> m ()
setClientAddress sessId addr = do
    unsubscribeAddr sessId
    csClients . at sessId . _Just . ccAddress .= addr
    whenJust addr $ subscribeAddr sessId

setClientBlock
    :: MonadState ConnectionsState m
    => SessionId -> Maybe ChainDifficulty -> m ()
setClientBlock sessId pId = do
    csClients . at sessId . _Just . ccBlock .= pId
    subscribeBlocks sessId

subscribeAddr
    :: MonadState ConnectionsState m
    => SessionId -> Address -> m ()
subscribeAddr i addr =
    csAddressSubscribers . at addr %= Just .
    (maybe (S.singleton i) (S.insert i))

unsubscribeAddr
    :: MonadState ConnectionsState m
    => SessionId -> m ()
unsubscribeAddr i = do
    addr <- preuse $ csClients . at i . _Just . ccAddress
    whenJust (join addr) unsubscribeDo
  where
    unsubscribeDo a = csAddressSubscribers . at a %= fmap (S.delete i)

subscribeBlocks
    :: MonadState ConnectionsState m
    => SessionId -> m ()
subscribeBlocks i = csBlocksSubscribers %= S.insert i

unsubscribeBlocks
    :: MonadState ConnectionsState m
    => SessionId -> m ()
unsubscribeBlocks i = csBlocksSubscribers %= S.delete i

unsubscribeFully
    :: MonadState ConnectionsState m
    => SessionId -> m ()
unsubscribeFully i = unsubscribeBlocks i >> unsubscribeAddr i
