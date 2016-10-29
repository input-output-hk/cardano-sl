{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Peer discovery

module Pos.DHT (
    DHTException (..),
    DHTKey,
    dhtKeyBytes,
    DHTData,
    DHTNode (..),
    DHTNodeType (..),
    MonadDHT (..),
    MonadMessageDHT (..),
    MonadResponseDHT (..),
    DHTResponseT,
    getDHTResponseT,
    randomDHTKey,
    bytesToDHTKey,
    dhtNodeType,
    WithDefaultMsgHeader (..),
    ListenerDHT (..),
    withDhtLogger,
    filterByNodeType,
    joinNetworkNoThrow
) where

import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow,
                                            catch, throwM)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Logging  (LoggerName,
                                            WithNamedLogger (modifyLoggerName),
                                            logInfo, logWarning)
import           Control.TimeWarp.Rpc      (Message, MonadDialog, MonadTransfer,
                                            NetworkAddress, ResponseT, replyH,
                                            sendH)
import           Control.TimeWarp.Timed    (MonadTimed, ThreadId)
import           Data.Binary               (Binary, Put)
import qualified Data.ByteString           as BS
import           Data.Proxy                (Proxy (Proxy))
import           Data.Text.Buildable       (Buildable (..))
import           Data.Text.Lazy            (unpack)
import           Data.Text.Lazy.Builder    (toLazyText)
import           Formatting                (bprint, int, sformat, shown, (%))
import qualified Formatting                as F
import           Pos.Crypto.Random         (secureRandomBS)
import           Prelude                   (show)
import           Serokell.Util.Text        (listBuilderJSON)
import           Universum                 hiding (ThreadId, catch, show)

import           Pos.Constants             (neighborsSendThreshold)

import qualified Serokell.Util.Base64      as B64

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

class WithDefaultMsgHeader m where
  defaultMsgHeader :: Message r => r -> m Put

class MonadDHT m => MonadMessageDHT m where

    sendToNetwork :: Message r => r -> m ()

    sendToNode :: Message r => NetworkAddress -> r -> m ()

    sendToNeighbors :: Message r => r -> m Int

    default sendToNode :: (Message r, WithDefaultMsgHeader m, MonadDialog m) => NetworkAddress -> r -> m ()
    sendToNode addr msg = do
        header <- defaultMsgHeader msg
        sendH addr header msg

    default sendToNeighbors :: (Message r, WithNamedLogger m, MonadCatch m, MonadIO m) => r -> m Int
    sendToNeighbors = defaultSendToNeighbors

class MonadMessageDHT m => MonadResponseDHT m where

  replyToNode :: Message r => r -> m ()

data ListenerDHT m =
    forall r . Message r => ListenerDHT (r -> DHTResponseT m ())

defaultSendToNeighbors
    :: ( MonadMessageDHT m
       , Message r
       , WithNamedLogger m
       , MonadCatch m
       , MonadIO m
       )
    => r -> m Int
defaultSendToNeighbors msg = do
    nodes <- filterByNodeType DHTFull <$> getKnownPeers
    succeed <- sendToNodes nodes
    succeed' <- if succeed < neighborsSendThreshold
                   then (+) succeed <$> do
                     nodes' <- discoverPeers DHTFull
                     sendToNodes $ filter (isJust . flip find nodes . (==)) nodes'
                   else return succeed
    when (succeed' < neighborsSendThreshold) $
      logWarning $ sformat ("Send to only " % int % " nodes out, threshold is " % int) succeed' neighborsSendThreshold
    return succeed'
  where
    -- TODO make this function asynchronous after presenting some `MonadAsync` constraint
    sendToNodes nodes = length . filter identity <$> mapM send' nodes
    send' node = (sendToNode (dhtAddr node) msg >> return True) `catch` handleE
      where
        handleE (e :: SomeException) = do
          logInfo $ sformat ("Error sending message to " % F.build % ": " % shown) node e
          return False

newtype DHTData = DHTData ()
  deriving (Eq, Ord, Binary, Show)

-- DHTKey should be strictly 20-byte long
newtype DHTKey = DHTKey { dhtKeyBytes :: BS.ByteString }
  deriving (Eq, Ord, Binary)

instance Buildable DHTKey where
    build key@(DHTKey bs) = buildType (dhtNodeType key)
                `mappend` build ' '
                `mappend` build (B64.encodeUrl bs)
      where
        buildType Nothing  = "<Unknown type>"
        buildType (Just s) = build s

instance Show DHTKey where
  show = unpack . toLazyText . build

-- Node type is determined by first byte of key
data DHTNodeType
  -- node which participates only in supporting DHT, i.e. not a part of PoS communication
  = DHTSupporter
  -- full node, i.e. fully participating in both DHT supporting and PoS
  | DHTFull
  -- client node (for SPV). Key idea is that clients, being a part of DHT, are rarely queried
  | DHTClient
  deriving (Eq, Ord, Show)

instance Buildable DHTNodeType where
  build = build . show

dhtNodeType :: DHTKey -> Maybe DHTNodeType
dhtNodeType (DHTKey bs) = impl $ BS.head bs
  where
    impl 0x00 = Just DHTSupporter
    impl 0x30 = Just DHTFull
    impl 0xF0 = Just DHTClient
    impl _    = Nothing

typeByte :: DHTNodeType -> Word8
typeByte DHTSupporter = 0x00
typeByte DHTFull      = 0x30
typeByte DHTClient    = 0xF0

bytesToDHTKey :: IsString s => BS.ByteString -> Either s DHTKey
bytesToDHTKey bs = if BS.length bs /= 20
                      then Left "Key ength must be exactly 20 bytes"
                      else Right $ DHTKey bs

randomDHTKey :: MonadIO m => DHTNodeType -> m DHTKey
randomDHTKey type_ = (DHTKey . BS.cons (typeByte type_)) <$> secureRandomBS 19

data DHTNode = DHTNode { dhtAddr   :: NetworkAddress
                       , dhtNodeId :: DHTKey
                       }
  deriving (Eq, Ord, Show)
instance Buildable DHTNode where
    build (DHTNode (peerHost, peerPort) key)
      = bprint (F.build % " at " % F.build % ":" % F.build)
               key
               (show peerHost)
               peerPort

instance Buildable [DHTNode] where
    build = listBuilderJSON

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

newtype DHTResponseT m a = DHTResponseT { getDHTResponseT :: (ResponseT m a) }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans,
                MonadThrow, MonadCatch, MonadMask,
                MonadState s, WithDefaultMsgHeader,
                WithNamedLogger, MonadTimed, MonadTransfer, MonadDHT, MonadMessageDHT)

type instance ThreadId (DHTResponseT m) = ThreadId m

instance (WithDefaultMsgHeader m, MonadMessageDHT m, MonadDialog m, MonadIO m) => MonadResponseDHT (DHTResponseT m) where
  replyToNode msg = do
    header <- defaultMsgHeader msg
    DHTResponseT $ replyH header msg

filterByNodeType :: DHTNodeType -> [DHTNode] -> [DHTNode]
filterByNodeType type_ = filter (\n -> dhtNodeType (dhtNodeId n) == Just type_)

joinNetworkNoThrow :: (MonadDHT m, MonadCatch m, MonadIO m, WithNamedLogger m) => [DHTNode] -> m ()
joinNetworkNoThrow peers = joinNetwork peers `catch` handleJoinE
  where
    handleJoinE AllPeersUnavailable =
        logInfo $ sformat ("Not connected to any of peers " % F.build) peers
    handleJoinE e = throwM e
