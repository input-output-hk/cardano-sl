{-# LANGUAGE ScopedTypeVariables       #-}

module Pos.DHT.Class.MonadDHT (
    DHTException (..),
    MonadDHT (..),
    withDhtLogger
) where

import           Control.TimeWarp.Rpc      (ResponseT)
import           Data.Proxy                (Proxy (Proxy))
import           System.Wlog               (HasLoggerName (modifyLoggerName),
                                            LoggerName)
import           Universum

import           Pos.DHT.Types             (DHTNode (..), DHTNodeType (..), DHTKey)

-- | Monad for Distributed Hash Table operations.
class Monad m => MonadDHT m where
    joinNetwork :: [DHTNode] -> m ()

    -- | Peer discovery: query DHT for random key
    -- Processing request, node will discover few other nodes
    -- We return these newly discovered nodes among with already known
    -- (List of known nodes is updated as well)
    discoverPeers :: DHTNodeType -> m [DHTNode]

    getKnownPeers :: m [DHTNode]

    currentNodeKey :: m DHTKey

    dhtLoggerName :: Proxy m -> LoggerName
    -- dhtLoggerName Proxy = "MonadDHT"

-- | Specialized logger name for DHT monad.
dhtLoggerNameM :: forall m . MonadDHT m => m LoggerName
dhtLoggerNameM = pure $ dhtLoggerName (Proxy :: Proxy m)

-- | Perform some action using 'dhtLoggerName'.
withDhtLogger
    :: (HasLoggerName m, MonadDHT m)
    => m a -> m a
withDhtLogger action = do
    subName <- dhtLoggerNameM
    modifyLoggerName (<> subName) action
instance MonadDHT m => MonadDHT (ReaderT r m) where
    joinNetwork = lift . joinNetwork
    discoverPeers = lift . discoverPeers
    getKnownPeers = lift getKnownPeers
    currentNodeKey = lift currentNodeKey
    dhtLoggerName  = dhtLoggerName . fromRProxy
      where
        fromRProxy :: Proxy (ReaderT r m) -> Proxy m
        fromRProxy _ = Proxy

instance MonadDHT m => MonadDHT (ResponseT m) where
    discoverPeers = lift . discoverPeers
    getKnownPeers = lift getKnownPeers
    currentNodeKey = lift currentNodeKey
    joinNetwork = lift . joinNetwork
    dhtLoggerName _ = dhtLoggerName (Proxy :: Proxy m)

-- | Data type for DHT exceptions.
data DHTException = NodeDown | AllPeersUnavailable
  deriving (Show, Typeable)

instance Exception DHTException
