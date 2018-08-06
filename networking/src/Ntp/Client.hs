{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module implements functionality of NTP client.

module Ntp.Client
    ( NtpConfiguration (..)
    , NtpClientSettings (..)
    , ntpClientSettings
    , NtpStatus (..)
    , withNtpClient
    ) where

import           Universum hiding (Last, catch)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, concurrently_, race)
import           Control.Concurrent.STM (TVar, check, modifyTVar', retry)
import           Control.Exception (Exception, IOException, catch, handle)
import           Control.Monad (forever)
import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON,
                     genericToJSON)
import           Data.Aeson.Options (defaultOptions)
import           Data.Binary (decodeOrFail)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup (Last (..))
import           Data.These (These (..))
import           Data.Time.Units (Microsecond, TimeUnit, fromMicroseconds,
                     toMicroseconds)
import           Data.Typeable (Typeable)
import           Formatting (sformat, shown, (%))
import qualified Network.Socket as Socket
import           Network.Socket.ByteString (recvFrom)
import qualified Pos.Util.Log as Log
import           Pos.Util.Trace.Named (TraceNamed, appendName, logDebug,
                     logInfo, logMessage, logWarning)

import           Ntp.Packet (NtpOffset, NtpPacket (..), clockOffset,
                     mkNtpPacket, ntpPacketSize)
import           Ntp.Util (AddrFamily (..), Addresses, Sockets,
                     WithAddrFamily (..), createAndBindSock, foldThese,
                     resolveNtpHost, runWithAddrFamily, sendPacket,
                     udpLocalAddresses)

data NtpStatus =
      -- | The difference between NTP time and local system time
      NtpDrift NtpOffset
      -- | NTP client has send requests to the servers
    | NtpSyncPending
      -- | NTP is not available: the client has not received any respond within
      -- `ntpResponseTimeout` or NTP was not configured.
    | NtpSyncUnavailable deriving (Eq, Show)

data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ list of servers addresses
    , ntpResponseTimeout :: Microsecond
      -- ^ delay between making requests and response collection
    , ntpPollDelay       :: Microsecond
      -- ^ how long to wait between to send requests to the servers
    , ntpSelection       :: NonEmpty NtpOffset -> NtpOffset
      -- ^ way to sumarize results received from different servers.
      -- this may accept list of lesser size than @length ntpServers@ in case
      -- some servers failed to respond in time, but never an empty list
    }

data NtpClient = NtpClient
    { ncSockets  :: TVar Sockets
      -- ^ Ntp client sockets: ipv4 / ipv6 / both.
    , ncState    :: TVar [NtpOffset]
      -- ^ List of ntp offsets and origin times (i.e. time when a request was
      -- send) received from ntp servers since last polling interval.
    , ncStatus   :: TVar NtpStatus
      -- ^ Ntp status: holds `NtpOffset` or a status of ntp client:
      -- `NtpSyncPending`, `NtpSyncUnavailable`.  It is computed from `ncState`
      -- once all responses arrived.
    , ncSettings :: NtpClientSettings
      -- ^ Ntp client configuration.
    }

data NtpConfiguration = NtpConfiguration
    {
      ntpcServers         :: [String]
      -- ^ List of DNS names of ntp servers
    , ntpcResponseTimeout :: !Integer
      -- ^ how long to await for responses from ntp servers (in microseconds)
    , ntpcPollDelay       :: !Integer
      -- ^ how long to wait between sending requests to the ntp servers (in
      -- microseconds)
    } deriving (Show, Generic)

instance FromJSON NtpConfiguration where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON NtpConfiguration where
    toJSON = genericToJSON defaultOptions

ntpClientSettings :: NtpConfiguration -> NtpClientSettings
ntpClientSettings NtpConfiguration {..} = NtpClientSettings
    { ntpServers         = ntpcServers
    , ntpResponseTimeout = fromMicroseconds $ ntpcResponseTimeout
    , ntpPollDelay       = fromMicroseconds $ ntpcPollDelay
    , ntpSelection       = minimum . NE.map abs
    -- ^ Take minmum of received offsets.
    }

mkNtpClient :: NtpClientSettings -> TVar NtpStatus -> Sockets -> IO NtpClient
mkNtpClient ncSettings ncStatus sock = liftIO $ do
    ncSockets <- newTVarIO sock
    ncState   <- newTVarIO []
    return NtpClient{..}

data NoHostResolved = NoHostResolved
    deriving (Show, Typeable)

instance Exception NoHostResolved

-- |
-- Update @'ncStatus'@ according to received responses.
updateStatus'
    :: TraceNamed IO
    -> NtpClient
    -> ([NtpOffset] -> (NtpStatus, (Log.Severity, Text)))
    -> IO ()
updateStatus' logTrace cli fn = do
    (offset, (sev, msg)) <- fn <$> readTVarIO (ncState cli)
    logMessage logTrace sev msg
    atomically $ writeTVar (ncStatus cli) offset

updateStatus :: TraceNamed IO -> NtpClient -> IO ()
updateStatus logTrace cli = updateStatus' logTrace cli fn
    where
    fn :: [NtpOffset]
       -> (NtpStatus, (Log.Severity, Text))
    fn [] = ( NtpSyncUnavailable
            , (Log.Warning, "ntp client haven't received any response")
            )
    fn offsets =
        let offset = ntpSelection (ncSettings cli) $ NE.fromList $ offsets
        in ( NtpDrift offset
           , (Log.Info, sformat ("Evaluated clock offset "%shown%"mcs") offset)
           )

-- |
-- Every `ntpPollDelay` we send a request to the list of `ntpServers`.  Before
-- sending a request, we put `NtpSyncPending` to `ncState`.  After sending
-- all requests we wait until either all servers responded or
-- `ntpResponseTimeout` passesed.  If at least one server responded
-- `handleCollectedResponses` will update `ncStatus` in `NtpClient` with a new
-- drift.
sendLoop :: TraceNamed IO -> NtpClient -> [Addresses] -> IO ()
sendLoop logTrace cli addrs = do
    let respTimeout = ntpResponseTimeout (ncSettings cli)
    let poll        = ntpPollDelay (ncSettings cli)

    -- send packets and wait until end of poll delay
    sock <- atomically $ readTVar $ ncSockets cli
    pack <- mkNtpPacket
    sendPacket logTrace sock pack addrs

    _ <- timeout respTimeout waitForResponses
    updateStatus logTrace cli
    -- after @'updateStatus'@ @'ntpStatus'@ is guaranteed to be
    -- different from @'NtpSyncPending'@, now we can wait until it was
    -- changed back to @'NtpSyncPending'@ to force a request.
    _ <- timeout poll waitForRequest

    -- reset state & status before next loop
    atomically $ writeTVar (ncState cli) []
    atomically $ writeTVar (ncStatus cli) NtpSyncPending

    sendLoop logTrace cli addrs

    where
        waitForResponses = do
            atomically $ do
                resps <- readTVar $ ncState cli
                let svs = length $ ntpServers $ ncSettings cli
                when (length resps < svs)
                    retry
            logDebug logTrace "collected all responses"

        -- Wait for a request to force an ntp check.
        waitForRequest =
            atomically $ do
                status <- readTVar $ ncStatus cli
                check (status == NtpSyncPending)
                return ()


-- |
-- Start listening for responses on the socket @'ncSockets'@
startReceive :: TraceNamed IO -> NtpClient -> IO ()
startReceive logTrace cli =
    atomically (readTVar $ ncSockets cli) >>= \case
        These (Last (WithIPv6 sock_ipv6)) (Last (WithIPv4 sock_ipv4)) ->
            loop IPv6 sock_ipv6
            `concurrently_`
            loop IPv4 sock_ipv4
        This (Last (WithIPv6 sock_ipv6)) ->
            loop IPv6 sock_ipv6
        That (Last (WithIPv4 sock_ipv4)) ->
            loop IPv4 sock_ipv4
    where
    -- Receive responses from the network and update NTP client state.
    loop :: AddrFamily -> Socket.Socket -> IO ()
    loop addressFamily sock
        = handle (handleIOException addressFamily) $ forever $ do
            (bs, _) <- recvFrom sock ntpPacketSize
            case decodeOrFail $ LBS.fromStrict bs of
                Left  (_, _, err)    ->
                    logWarning logTrace $ sformat ("Error while receiving time: "%shown) err
                Right (_, _, packet) ->
                    handleNtpPacket packet

    -- Restart the @loop@ in case of errors; wait 5s before recreating the
    -- socket.
    handleIOException
        :: AddrFamily
        -> IOException
        -> IO ()
    handleIOException addressFamily e = do
        logDebug logTrace $ sformat ("startReceive failed with reason: "%shown) e
        threadDelay 5000000
        udpLocalAddresses >>= createAndBindSock logTrace addressFamily >>= \case
            Nothing   -> logWarning logTrace "recreating of sockets failed (retrying)" >> handleIOException addressFamily e
            Just sock -> do
                atomically $ modifyTVar' (ncSockets cli) (\s -> s <> sock)
                case sock of
                    This (Last sock_)
                        -> loop addressFamily $ runWithAddrFamily sock_
                    That (Last sock_)
                        -> loop addressFamily $ runWithAddrFamily sock_
                    These _ _
                        -> error "NtpClient: startReceive: impossible"

    -- Compute the clock offset based on current time and record it in the NTP
    -- client state.   A packet will be disgarded if it came after
    -- @'ntpResponseTimeout'@.
    handleNtpPacket
        :: NtpPacket
        -> IO ()
    handleNtpPacket packet = do
        logDebug logTrace $ sformat ("Got packet "%shown) packet

        clockOffset (ntpResponseTimeout $ ncSettings cli) packet >>= \case
            Nothing ->
                logWarning logTrace "Response was too late: discarding it."
            Just offset -> do
                logDebug logTrace $ sformat ("Received time delta "%shown%"mcs")
                    (toMicroseconds offset)
                atomically $ modifyTVar' (ncState cli) ( offset : )

-- |
-- Spawn NTP client which will send request to NTP servers every @'ntpPollDelay'@
-- and will listen for responses.  The @'ncStatus'@ will be updated every
-- @'ntpPollDelay'@ with the most recent value.  It should be run in a separate
-- thread, since it will block infinitely.
spawnNtpClient :: TraceNamed IO -> NtpClientSettings -> TVar NtpStatus -> IO ()
spawnNtpClient logTrace settings ncStatus = do
    logInfo logTrace "starting"
    bracket (mkSockets logTrace settings) closeSockets $ \sock -> do
        cli <- mkNtpClient settings ncStatus sock

        addrs <- catMaybes <$> traverse (resolveNtpHost logTrace) (ntpServers settings)
        when (null addrs) $ throwM NoHostResolved
        -- TODO
        -- we should start listening for requests when we send something, since
        -- we're not expecting anything to come unless we send something.  This
        -- way we could simplify the client and remove `ncState` mutable cell.
        startReceive logTrace cli
            `concurrently_` sendLoop logTrace cli addrs
            `concurrently_` logInfo logTrace "started"
    where
    closeSockets :: Sockets -> IO ()
    closeSockets sockets = do
        foldThese $ bimap fn fn sockets
        logInfo logTrace "stopped"

    fn :: Last (WithAddrFamily t Socket.Socket) -> IO ()
    fn (Last sock) = Socket.close $ runWithAddrFamily sock

-- |
-- Run NTP client in a separate thread; it returns a mutable cell which holds
-- @'NtpStatus'@.
--
-- This function should be called once, it will run an NTP client in a new
-- thread until the program terminates.
withNtpClient :: MonadIO m => TraceNamed IO -> NtpClientSettings -> m (TVar NtpStatus)
withNtpClient logTrace0 ntpSettings = do
    let logTrace = appendName "withNtpClient" logTrace0
    liftIO $ logInfo logTrace "withNtpClient"
    ncStatus <- newTVarIO NtpSyncPending
    -- using async so the NTP thread will be left running even if the parent
    -- thread finished.
    _ <- liftIO $ async (spawnNtpClient logTrace ntpSettings ncStatus)
    return ncStatus

-- Try to create IPv4 and IPv6 socket.
mkSockets :: TraceNamed IO -> NtpClientSettings -> IO Sockets
mkSockets logTrace settings =
    doMkSockets `catch` handleIOException >>= \case
        Option (Just sock) -> pure sock
        Option Nothing     -> do
            logWarning logTrace "Couldn't create both IPv4 and IPv6 socket, retrying in 5 sec..."
            threadDelay 5000000
            mkSockets logTrace settings
  where
    doMkSockets :: IO (Option Sockets)
    doMkSockets = do
        addrs <- udpLocalAddresses
        (<>) <$> (Option <$> createAndBindSock logTrace IPv4 addrs)
             <*> (Option <$> createAndBindSock logTrace IPv6 addrs)

    handleIOException :: IOException -> IO (Option Sockets)
    handleIOException e = do
        logWarning logTrace $
            sformat ("Failed to create sockets, retrying in 5 sec... (reason: "%shown%")")
            e
        threadDelay 5000000
        doMkSockets

timeout :: TimeUnit t => t -> IO a -> IO (Maybe a)
timeout t io = rightToMaybe <$> race (threadDelay (fromIntegral (toMicroseconds t))) io
