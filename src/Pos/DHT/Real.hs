{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Pos.DHT.Real
       ( KademliaDHT
       , runKademliaDHT
       , KademliaDHTConfig(..)
       , KademliaDHTInstanceConfig(..)
       , KademliaDHTInstance
       , startDHTInstance
       , stopDHTInstance
       ) where

import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Concurrent.STM          (STM, TVar, atomically, modifyTVar,
                                                  newTVar, readTVar, swapTVar, writeTVar)
import           Control.Monad.Catch             (Handler (..), MonadCatch, MonadMask,
                                                  MonadThrow, catchAll, catches, throwM)
import           Control.Monad.Trans.Class       (MonadTrans)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Control.TimeWarp.Logging        (WithNamedLogger, logDebug, logError,
                                                  logInfo, logWarning, usingLoggerName)
import           Control.TimeWarp.Rpc            (BinaryP (..), Binding (..),
                                                  ListenerH (..), MonadDialog,
                                                  MonadResponse, MonadTransfer (..),
                                                  NetworkAddress, RawData (..),
                                                  TransferException (..), hoistRespCond,
                                                  listenR, sendH, sendR)
import           Control.TimeWarp.Timed          (MonadTimed, ThreadId, fork, killThread,
                                                  ms, sec)
import           Data.Binary                     (Binary, decodeOrFail, encode)
import qualified Data.ByteString                 as BS
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import qualified Data.Cache.LRU                  as LRU
import           Data.Hashable                   (hash)
import qualified Data.HashMap.Strict             as HM
import           Data.Text                       (Text)
import           Formatting                      (build, int, sformat, shown, (%))
import qualified Network.Kademlia                as K
import           Pos.DHT                         (DHTData, DHTException (..), DHTKey,
                                                  DHTMsgHeader (..), DHTNode (..),
                                                  DHTNodeType (..), DHTResponseT (..),
                                                  ListenerDHT (..), MonadDHT (..),
                                                  MonadMessageDHT (..),
                                                  MonadResponseDHT (closeResponse),
                                                  WithDefaultMsgHeader (..),
                                                  defaultSendToNeighbors,
                                                  defaultSendToNode, filterByNodeType,
                                                  joinNetworkNoThrow, randomDHTKey,
                                                  withDhtLogger)
import           Pos.Util                        (runWithRandomIntervals)
import           Universum                       hiding (fromStrict, mapConcurrently,
                                                  toStrict)

toBSBinary :: Binary b => b -> BS.ByteString
toBSBinary = toStrict . encode

fromBSBinary :: Binary b => BS.ByteString -> Either [Char] (b, BS.ByteString)
fromBSBinary bs =
    case decodeOrFail $ fromStrict bs of
        Left (_, _, errMsg)  -> Left errMsg
        Right (rest, _, res) -> Right (res, toStrict rest)

instance K.Serialize DHTData where
  toBS = toBSBinary
  fromBS = fromBSBinary

instance K.Serialize DHTKey where
  toBS = toBSBinary
  fromBS = fromBSBinary

type DHTHandle = K.KademliaInstance DHTKey DHTData

data KademliaDHTInstance = KademliaDHTInstance
    { kdiHandle          :: !DHTHandle
    , kdiKey             :: !DHTKey
    , kdiInitialPeers    :: ![DHTNode]
    , kdiExplicitInitial :: !Bool
    }

data KademliaDHTContext m = KademliaDHTContext
    { kdcDHTInstance_         :: !KademliaDHTInstance
    , kdcAuxClosers           :: !(TVar [m ()])
    , kdcListenByBinding      :: !(Binding -> KademliaDHT m (IO ()))
    , kdcStopped              :: !(TVar Bool)
    , kdcNoCacheMessageNames_ :: ![Text]
    }

data KademliaDHTConfig m = KademliaDHTConfig
    { kdcPort                :: !Word16
    , kdcListeners           :: ![ListenerDHT (KademliaDHT m)]
    , kdcMessageCacheSize    :: !Int
    , kdcEnableBroadcast     :: !Bool
    , kdcNoCacheMessageNames :: ![Text]
    , kdcDHTInstance         :: !KademliaDHTInstance
    }
data KademliaDHTInstanceConfig = KademliaDHTInstanceConfig
    { kdcPort            :: !Word16
    , kdcKeyOrType       :: !(Either DHTKey DHTNodeType)
    , kdcInitialPeers    :: ![DHTNode]
    , kdcExplicitInitial :: !Bool
    }

newtype KademliaDHT m a = KademliaDHT { unKademliaDHT :: ReaderT (KademliaDHTContext m) m a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO,
             MonadMask, WithNamedLogger, MonadTimed, MonadDialog p, MonadResponse)

--instance MonadTransControl KademliaDHT where
--    type StT KademliaDHT a = StT (ReaderT (KademliaDHTContext m)) a
--    liftWith = defaultLiftWith KademliaDHT unKademliaDHT
--    restoreT = defaultRestoreT KademliaDHT
--
--instance MonadBaseControl IO m => MonadBaseControl IO (KademliaDHT m) where
--    type StM (KademliaDHT m) a = ComposeSt KademliaDHT m a
--    liftBaseWith     = defaultLiftBaseWith
--    restoreM         = defaultRestoreM

instance MonadTransfer m => MonadTransfer (KademliaDHT m) where
    sendRaw addr req = lift $ sendRaw addr req
    listenRaw binding sink =
        KademliaDHT $ listenRaw binding $ hoistRespCond unKademliaDHT sink
    close = lift . close

instance Applicative m => WithDefaultMsgHeader (KademliaDHT m) where
    defaultMsgHeader _ = do
        -- *-- Caches are disabled now for non-broadcast messages
        --     uncomment lines below to enable them
        --noCacheNames <- KademliaDHT $ asks kdcNoCacheMessageNames_
        --let header =
        --        SimpleHeader . isJust . find (== messageName' msg) $
        --        noCacheNames
        let header = SimpleHeader True
        --withDhtLogger $
        --    logDebug $
        --    sformat
        --        ("Preparing message " % stext % ": header " % shown)
        --        (messageName' msg)
        --        header
        pure header

instance MonadTrans KademliaDHT where
  lift = KademliaDHT . lift

type instance ThreadId (KademliaDHT m) = ThreadId m

runKademliaDHT
    :: (WithNamedLogger m, MonadIO m, MonadTimed m, MonadDialog BinaryP m, MonadMask m, MonadBaseControl IO m)
    => KademliaDHTConfig m -> KademliaDHT m a -> m a
runKademliaDHT kdc@(KademliaDHTConfig {..}) action =
    startDHT kdc >>= runReaderT (unKademliaDHT action')
  where
    action' =
        (startMsgThread
          >> logDebug "running kademlia dht messager"
          >> action'')
        `finally`
        (logDebug "stopping kademlia messager"
          >> stopDHT >> logDebug "kademlia messager stopped")
    action'' = do
      joinNetworkNoThrow (kdiInitialPeers $ kdcDHTInstance)
      startRejoinThread
      action
    startRejoinThread = do
      tvar <- KademliaDHT $ asks kdcAuxClosers
      tid <- fork $ runWithRandomIntervals (ms 500) (sec 5) rejoinNetwork
      liftIO . atomically $ modifyTVar tvar (killThread tid:)
    startMsgThread = do
      (tvar, listenByBinding) <-
          KademliaDHT $ (,) <$> asks kdcAuxClosers <*> asks kdcListenByBinding
      closer <- listenByBinding $ AtPort kdcPort
      liftIO . atomically $ modifyTVar tvar (liftIO closer:)

stopDHT :: (MonadTimed m, MonadIO m) => KademliaDHT m ()
stopDHT = do
    (closersTV, stoppedTV) <- KademliaDHT $ (,)
            <$> asks kdcAuxClosers
            <*> asks kdcStopped
    liftIO . atomically $ writeTVar stoppedTV True
    closers <- liftIO . atomically $ swapTVar closersTV []
    lift $ sequence_ closers

stopDHTInstance :: (MonadTimed m, MonadIO m) => KademliaDHTInstance -> m ()
stopDHTInstance KademliaDHTInstance {..} = liftIO $ K.close kdiHandle

startDHTInstance
    :: ( MonadTimed m
       , MonadIO m
       , MonadDialog BinaryP m
       , WithNamedLogger m
       , MonadCatch m
       , MonadBaseControl IO m
       )
    => KademliaDHTInstanceConfig -> m KademliaDHTInstance
startDHTInstance KademliaDHTInstanceConfig {..} = do
    kdiKey <- either pure randomDHTKey kdcKeyOrType
    kdiHandle <-
        (liftIO $
        K.createL
            (fromInteger . toInteger $ kdcPort)
            kdiKey
            (log' logDebug)
            (log' logError))
          `catchAll`
        (\e ->
           do logError $ sformat
                  ("Error launching kademlia at port " % int % ": " % shown) kdcPort e
              throwM e)
    let kdiInitialPeers = kdcInitialPeers
    let kdiExplicitInitial = kdcExplicitInitial
    pure $ KademliaDHTInstance {..}
  where
    log' logF =  usingLoggerName ("kademlia" <> "messager") . logF . toText

startDHT
    :: ( MonadTimed m
       , MonadIO m
       , MonadDialog BinaryP m
       , WithNamedLogger m
       , MonadCatch m
       , MonadBaseControl IO m
       )
    => KademliaDHTConfig m -> m (KademliaDHTContext m)
startDHT KademliaDHTConfig {..} = do
    kdcStopped <- liftIO . atomically $ newTVar False
    kdcAuxClosers <- liftIO . atomically $ newTVar []
    msgCache <- liftIO . atomically $
        newTVar (LRU.newLRU (Just $ toInteger kdcMessageCacheSize) :: LRU.LRU Int ())
    let kdcListenByBinding binding = do
            closer <- listenR binding (convert <$> kdcListeners)
                (convert' $ rawListener kdcEnableBroadcast msgCache kdcStopped)
            logInfo $ sformat ("Listening on binding " % shown) binding
            return closer
    logInfo $ sformat ("Launching Kademlia, noCacheMessageNames=" % shown) kdcNoCacheMessageNames
    let kdcNoCacheMessageNames_ = kdcNoCacheMessageNames
    let kdcDHTInstance_ = kdcDHTInstance
    pure $ KademliaDHTContext {..}
  where
    convert :: ListenerDHT m -> ListenerH BinaryP DHTMsgHeader m
    convert (ListenerDHT f) = ListenerH $ \(_, m) -> getDHTResponseT $ f m
    log' logF =  usingLoggerName ("kademlia" <> "messager") . logF . toText
    convert' handler = getDHTResponseT . handler

-- | Return 'True' if the message should be processed, 'False' if only
-- broadcasted
rawListener
    :: ( MonadBaseControl IO m
       , MonadCatch m
       , MonadDialog BinaryP m
       , MonadIO m
       , MonadTimed m
       , WithNamedLogger m
       )
    => Bool
    -> TVar (LRU.LRU Int ())
    -> TVar Bool
    -> (DHTMsgHeader, RawData)
    -> DHTResponseT (KademliaDHT m) Bool
rawListener enableBroadcast cache kdcStopped (h, rawData@(RawData raw)) = withDhtLogger $ do
    isStopped <- liftIO . atomically $ readTVar kdcStopped
    when isStopped $ do
        closeResponse
        throwM $ FatalError "KademliaDHT stopped"
    let mHash = hash raw
    ignoreMsg <- case h of
                   SimpleHeader True -> return False
                   _                 -> liftIO . atomically $ updCache cache mHash
    if ignoreMsg
       then logDebug $
                sformat ("Ignoring message " % shown % ", hash=" % int) h mHash
       else return ()

    -- If the message is in cache, we have already broadcasted it before, no
    -- need to do it twice
    when (not ignoreMsg && enableBroadcast) $
        case h of
            BroadcastHeader -> do
              logDebug $
                sformat ("Broadcasting message " % shown % ", hash=" % int) h mHash
              lift $ sendToNetworkR rawData
            SimpleHeader _  -> pure ()
    -- If the message wasn't in the cache, we want to process it too (not
    -- simply broadcast it)
    return (not ignoreMsg)

updCache :: TVar (LRU.LRU Int ()) -> Int -> STM Bool
updCache cacheTV dataHash = do
    cache <- readTVar cacheTV
    let (cache', mP) = dataHash `LRU.lookup` cache
    case mP of
      Just _ -> writeTVar cacheTV cache' >> pure True
      _      -> writeTVar cacheTV (LRU.insert dataHash () cache') >> pure False

sendToNetworkR
    :: ( MonadBaseControl IO m
       , WithNamedLogger m
       , MonadCatch m
       , MonadIO m
       , MonadDialog BinaryP m
       , MonadTimed m
       ) => RawData -> KademliaDHT m ()
sendToNetworkR = sendToNetworkImpl sendR

sendToNetworkImpl
    :: ( MonadBaseControl IO m
       , WithNamedLogger m
       , MonadCatch m
       , MonadIO m
       , MonadTimed m
       , MonadDialog BinaryP m
       ) => (NetworkAddress -> DHTMsgHeader -> msg -> KademliaDHT m ()) -> msg -> KademliaDHT m ()
sendToNetworkImpl sender msg = do
    logDebug "Sending message to network"
    void $ defaultSendToNeighbors seqConcurrentlyK (flip sender BroadcastHeader) msg

seqConcurrentlyK :: MonadBaseControl IO m => [KademliaDHT m a] -> KademliaDHT m [a]
seqConcurrentlyK = KademliaDHT . mapConcurrently unKademliaDHT

instance (MonadDialog BinaryP m, WithNamedLogger m, MonadCatch m, MonadIO m, MonadTimed m, MonadBaseControl IO m)
       => MonadMessageDHT (KademliaDHT m) where
    sendToNetwork = sendToNetworkImpl sendH
    sendToNeighbors = defaultSendToNeighbors seqConcurrentlyK sendToNode
    sendToNode addr msg = do
        defaultSendToNode addr msg
        listenOutbound >>= updateClosers
      where
        -- TODO [CSL-4] temporary code, to refactor to subscriptions (after TW-47)
        listenOutboundDo = KademliaDHT (asks kdcListenByBinding) >>= ($ AtConnTo addr)
        listenOutbound = listenOutboundDo `catches` [Handler handleAL, Handler handleTE]
        handleAL (AlreadyListeningOutbound _) = return $ pure ()
        handleTE e@(SomeException _) = do
            logDebug $ sformat ("Error listening outbound connection to " %
                                shown % ": " % build) addr e
            return $ pure ()
        updateClosers closer = KademliaDHT (asks kdcAuxClosers)
                            >>= \tvar -> (liftIO . atomically $ modifyTVar tvar (liftIO closer:))

rejoinNetwork :: (MonadIO m, WithNamedLogger m, MonadCatch m) => KademliaDHT m ()
rejoinNetwork = do
    peers <- getKnownPeers
    logDebug $ sformat ("rejoinNetwork: peers " % build) peers
    when (null peers) $ do
      logWarning "Empty known peer list"
      init <- KademliaDHT $ asks (kdiInitialPeers . kdcDHTInstance_)
      joinNetworkNoThrow init

instance (MonadIO m, MonadCatch m, WithNamedLogger m) => MonadDHT (KademliaDHT m) where

  joinNetwork [] = throwM AllPeersUnavailable
  joinNetwork nodes = do
      inst <- KademliaDHT $ asks (kdiHandle . kdcDHTInstance_)
      asyncs <- mapM (liftIO . async . joinNetwork' inst) nodes
      waitAnyUnexceptional asyncs >>= handleRes
    where
      handleRes (Just _) = pure ()
      handleRes _        = throwM AllPeersUnavailable

  discoverPeers type_ = do
    inst <- KademliaDHT $ asks (kdiHandle . kdcDHTInstance_)
    _ <- liftIO $ K.lookup inst =<< randomDHTKey type_
    filterByNodeType type_ <$> getKnownPeers

  getKnownPeers = do
      myId <- currentNodeKey
      (inst, initialPeers, explicitInitial) <- KademliaDHT $ (,,)
          <$> asks (kdiHandle . kdcDHTInstance_)
          <*> asks (kdiInitialPeers . kdcDHTInstance_)
          <*> asks (kdiExplicitInitial . kdcDHTInstance_)
      extendPeers myId (if explicitInitial then initialPeers else []) <$> liftIO (K.dumpPeers inst)
    where
      extendPeers myId initial
        = map snd
        . HM.toList
        . HM.delete myId
        . flip (foldr $ \n -> HM.insert (dhtNodeId n) n) initial
        . HM.fromList
        . map (\(toDHTNode -> n) -> (dhtNodeId n, n))


  currentNodeKey = KademliaDHT $ asks (kdiKey . kdcDHTInstance_)

  dhtLoggerName _ = "kademlia"

toDHTNode :: K.Node DHTKey -> DHTNode
toDHTNode n = DHTNode (fromKPeer . K.peer $ n) $ K.nodeId n

fromKPeer :: K.Peer -> NetworkAddress
fromKPeer K.Peer{..} = (encodeUtf8 peerHost, fromIntegral peerPort)

toKPeer :: NetworkAddress -> K.Peer
toKPeer (peerHost, peerPort) = K.Peer (decodeUtf8 peerHost) (fromIntegral peerPort)

-- TODO add TimedIO, WithLoggerName constraints and uncomment logging
joinNetwork' :: (MonadIO m, MonadThrow m) => DHTHandle -> DHTNode -> m ()
joinNetwork' inst node = do
  let node' = K.Node (toKPeer $ dhtAddr node) (dhtNodeId node)
  res <- liftIO $ K.joinNetwork inst node'
  case res of
    K.JoinSucces -> pure ()
    K.NodeDown   -> throwM NodeDown
    K.IDClash    -> pure () --logInfo $ sformat ("joinNetwork: node " % build % " already contains us") node

-- TODO move to serokell-core ?
waitAnyUnexceptional :: (MonadIO m, WithNamedLogger m) => [Async a] -> m (Maybe (Async a, a))
waitAnyUnexceptional asyncs = liftIO (waitAnyCatch asyncs) >>= handleRes
  where
    handleRes (async', Right res) = pure $ Just (async', res)
    handleRes (async', Left e) = do
      logWarning $ sformat ("waitAnyUnexceptional: caught error " % shown) e
      if null asyncs'
         then pure Nothing
         else waitAnyUnexceptional asyncs'
      where asyncs' = filter (/= async') asyncs
