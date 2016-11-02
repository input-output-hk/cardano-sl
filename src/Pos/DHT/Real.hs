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
       ) where

import           Control.Concurrent.Async.Lifted (mapConcurrently)
import           Control.Concurrent.STM          (STM, TVar, atomically, modifyTVar,
                                                  newTVar, readTVar, writeTVar)
import           Control.Monad.Catch             (Handler (..), MonadCatch, MonadMask,
                                                  MonadThrow, catchAll, catches, finally,
                                                  throwM)
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
import           Control.TimeWarp.Timed          (MonadTimed, ThreadId)
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
                                                  DHTNodeType (..),
                                                  DHTResponseT (getDHTResponseT),
                                                  ListenerDHT (..), MonadDHT (..),
                                                  MonadMessageDHT (..),
                                                  MonadResponseDHT (closeResponse),
                                                  WithDefaultMsgHeader (..),
                                                  defaultSendToNeighbors,
                                                  defaultSendToNode, filterByNodeType,
                                                  joinNetworkNoThrow, randomDHTKey,
                                                  withDhtLogger)
import           Universum                       hiding (Handler, catches, finally,
                                                  fromStrict, mapConcurrently, toStrict)

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

data KademliaDHTContext m = KademliaDHTContext
    { kdcHandle               :: DHTHandle
    , kdcKey                  :: DHTKey
    , kdcMsgThreadIds         :: TVar [IO ()]
    , kdcInitialPeers_        :: [DHTNode]
    , kdcListenByBinding      :: Binding -> KademliaDHT m (IO ())
    , kdcStopped              :: TVar Bool
    , kdcNoCacheMessageNames_ :: [Text]
    }

data KademliaDHTConfig m = KademliaDHTConfig
    { kdcPort                :: Word16
    , kdcListeners           :: [ListenerDHT (KademliaDHT m)]
    , kdcMessageCacheSize    :: Int
    , kdcEnableBroadcast     :: Bool
    , kdcKeyOrType           :: Either DHTKey DHTNodeType
    , kdcInitialPeers        :: [DHTNode]
    , kdcNoCacheMessageNames :: [Text]
    , kdcExplicitInitial     :: Bool
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

instance (MonadIO m, WithNamedLogger m, MonadCatch m) =>
         WithDefaultMsgHeader (KademliaDHT m) where
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
        (startMsgThread >> logDebug "running kademlia" >> action'')
        `finally`
        (logDebug "stopping kademlia" >> stopDHT >> logDebug "kademlia stopped")
    action'' = do
      joinNetworkNoThrow kdcInitialPeers
      action
    startMsgThread = do
      (tvar, listenByBinding) <-
          KademliaDHT $ (,) <$> asks kdcMsgThreadIds <*> asks kdcListenByBinding
      tId <- listenByBinding $ AtPort kdcPort
      liftIO . atomically $ modifyTVar tvar (tId:)

stopDHT :: (MonadTimed m, MonadIO m) => KademliaDHT m ()
stopDHT = do
    (kdcH, threadsTV, stoppedTV) <- KademliaDHT $ (,,)
            <$> asks kdcHandle
            <*> asks kdcMsgThreadIds
            <*> asks kdcStopped
    liftIO . atomically $ writeTVar stoppedTV True
    liftIO $ K.close kdcH
    threadIds <- liftIO . atomically $ readTVar threadsTV <* writeTVar threadsTV []
    liftIO $ sequence_ threadIds

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
    kdcKey <- either pure randomDHTKey kdcKeyOrType
    kdcHandle <-
        (liftIO $
        K.createL
            (fromInteger . toInteger $ kdcPort)
            kdcKey
            (log' logDebug)
            (log' logError))
          `catchAll`
        (\e ->
           do logError $ sformat
                  ("Error launching kademlia at port " % int % ": " % shown) kdcPort e
              throwM e)
    kdcStopped <- liftIO . atomically $ newTVar False
    kdcMsgThreadIds <- liftIO . atomically $ newTVar []
    let kdcInitialPeers_ = kdcInitialPeers
    msgCache <- liftIO . atomically $
        newTVar (LRU.newLRU (Just $ toInteger kdcMessageCacheSize) :: LRU.LRU Int ())
    let kdcListenByBinding binding = do
            closer <- listenR binding (convert <$> kdcListeners)
                (convert' $ rawListener kdcEnableBroadcast msgCache kdcStopped)
            logInfo $ sformat ("Listening on binding " % shown) binding
            return closer
    logInfo $ sformat ("Launching Kademlia, noCacheMessageNames=" % shown) kdcNoCacheMessageNames
    let kdcNoCacheMessageNames_ = kdcNoCacheMessageNames
    pure $ KademliaDHTContext {..}
  where
    convert :: ListenerDHT m -> ListenerH BinaryP DHTMsgHeader m
    convert (ListenerDHT f) = ListenerH $ \(_, m) -> getDHTResponseT $ f m
    log' logF =  usingLoggerName ("kademlia" <> "instance") . logF . toText
    convert' handler = getDHTResponseT . handler

-- | Return 'True' if the message should be processed, 'False' if only
-- broadcasted
rawListener
    :: (WithDefaultMsgHeader m
       ,MonadIO m
       ,MonadThrow m
       ,MonadDialog BinaryP m
       ,WithNamedLogger m
       ,MonadMessageDHT m)
    => Bool
    -> TVar (LRU.LRU Int ())
    -> TVar Bool
    -> (DHTMsgHeader, RawData)
    -> DHTResponseT m Bool
rawListener enableBroadcast cache kdcStopped (h, rawData@(RawData raw)) = withDhtLogger $ do
    isStopped <- liftIO . atomically $ readTVar kdcStopped
    when isStopped $ do
        closeResponse
        throwM $ FatalError "KademliaDHT stopped"
    let mHash = hash raw
    --logDebug $
    --    sformat ("Received message " % shown % ", hash=" % int) h mHash
    ignoreMsg <- case h of
                   SimpleHeader True -> return False
                   _                 -> liftIO . atomically $ updCache cache mHash
    if ignoreMsg
       then logDebug $
                sformat ("Ignoring message " % shown % ", hash=" % int) h mHash
       else return ()
       -- Uncomment to dump messages:
       -- else logDebug $ sformat ("Message: hash=" % int % " bytes=" % base64F) mHash raw


    -- If the message is in cache, we have already broadcasted it before, no
    -- need to do it twice
    when (not ignoreMsg && enableBroadcast) $
        case h of
            BroadcastHeader -> do
              logDebug $
                sformat ("Broadcasting message " % shown % ", hash=" % int) h mHash
              sendToNetworkR rawData
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


sendToNetworkR :: MonadDialog BinaryP m => RawData -> m ()
sendToNetworkR = sendToNetworkImpl sendR

sendToNetworkImpl :: (NetworkAddress -> DHTMsgHeader -> msg -> m ()) -> msg -> m ()
sendToNetworkImpl = notImplemented

instance (MonadDialog BinaryP m, WithNamedLogger m, MonadCatch m, MonadIO m, MonadTimed m, MonadBaseControl IO m)
       => MonadMessageDHT (KademliaDHT m) where
    sendToNetwork = sendToNetworkImpl sendH
    sendToNeighbors = defaultSendToNeighbors (KademliaDHT . mapConcurrently unKademliaDHT)
    sendToNode addr msg = do
        defaultSendToNode addr msg
        listenOutbound >>= updateTIds
      where
        -- TODO [CSL-4] temporary code, to refactor to subscriptions (after TW-47)
        listenOutboundDo = KademliaDHT (asks kdcListenByBinding) >>= ($ AtConnTo addr)
        listenOutbound = listenOutboundDo `catches` [Handler handleAL, Handler handleTE]
        handleAL (AlreadyListeningOutbound _) = return $ pure ()
        handleTE e@(SomeException _) = do
            logDebug $ sformat ("Error listening outbound connection to " %
                                shown % ": " % build) addr e
            return $ pure ()
        updateTIds tid = KademliaDHT (asks kdcMsgThreadIds)
                            >>= \tvar -> (liftIO . atomically $ modifyTVar tvar (tid:))

instance (MonadIO m, MonadCatch m, WithNamedLogger m) => MonadDHT (KademliaDHT m) where

  joinNetwork [] = throwM AllPeersUnavailable
  joinNetwork nodes = do
      inst <- KademliaDHT $ asks kdcHandle
      asyncs <- mapM (liftIO . async . joinNetwork' inst) nodes
      waitAnyUnexceptional asyncs >>= handleRes
    where
      handleRes (Just _) = pure ()
      handleRes _        = throwM AllPeersUnavailable

  discoverPeers type_ = do
    inst <- KademliaDHT $ asks kdcHandle
    peers <- getKnownPeers
    when (null peers) $ do
      logWarning "Empty known peer list"
      init <- KademliaDHT $ asks kdcInitialPeers_
      joinNetworkNoThrow init
    _ <- liftIO $ K.lookup inst =<< randomDHTKey type_
    filterByNodeType type_ <$> getKnownPeers

  getKnownPeers = do
      myId <- currentNodeKey
      (inst, initialPeers) <- KademliaDHT $ (,) <$> asks kdcHandle <*> asks kdcInitialPeers_
      extendPeers myId initialPeers <$> liftIO (K.dumpPeers inst)
    where
      extendPeers myId initial
        = map snd
        . HM.toList
        . HM.delete myId
        . flip (foldr $ \n -> HM.insert (dhtNodeId n) n) initial
        . HM.fromList
        . map (\(toDHTNode -> n) -> (dhtNodeId n, n))


  currentNodeKey = KademliaDHT $ asks kdcKey

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
