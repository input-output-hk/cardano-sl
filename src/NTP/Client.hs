{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}

-- | This module implements functionality of NTP client.

module NTP.Client
    ( startNtpClient
    , NtpClientSettings (..)
    , NtpStopButton (..)
    , ntpSingleShot
    , hoistNtpClientSettings
    ) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO,
                                              writeTVar)
import           Control.Lens                ((%=), (.=), _Just)
import           Control.Monad               (forM_, forever, unless, void, when)
import           Control.Monad.Catch         (Exception)
import           Control.Monad.State         (gets)
import           Control.Monad.Trans         (MonadIO (..))
import           Data.Binary                 (decodeOrFail, encode)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Default                (Default (..))
import           Data.List                   (sortOn)
import           Data.Maybe                  (isNothing)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Time.Units             (Microsecond, Second, toMicroseconds)
import           Data.Typeable               (Typeable)
import           Formatting                  (sformat, shown, (%))
import           Network.Socket              (AddrInfoFlag (AI_PASSIVE), SockAddr (..),
                                              Socket, SocketOption (IPv6Only, ReuseAddr),
                                              SocketType (Datagram), aNY_PORT,
                                              addrAddress, addrFamily, addrFlags,
                                              addrSocketType, bind, close, defaultHints,
                                              defaultProtocol, getAddrInfo,
                                              setSocketOption, socket)
import           Network.Socket.ByteString   (recvFrom, sendTo)
import           Prelude                     hiding (log)
import           Serokell.Util.Concurrent    (modifyTVarS, threadDelay)
import           System.Wlog                 (LoggerName, Severity (..), WithLogger,
                                              logMessage, modifyLoggerName)

import           Mockable.Class              (Mockable)
import           Mockable.Concurrent         (Delay, Fork, delay, fork)
import           Mockable.Exception          (Catch, Throw, catchAll, handleAll, throw)
import           NTP.Packet                  (NtpPacket (..), evalClockOffset,
                                              mkCliNtpPacket, ntpPacketSize)
import           NTP.Util                    (preferIPv6, resolveNtpHost)

data NtpClientSettings m = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ list of servers addresses
    , ntpHandler         :: (Microsecond, Microsecond) -> m ()
      -- ^ got time callback (margin, time when client sent request)
    , ntpLogName         :: LoggerName
      -- ^ logger name modifier
    , ntpResponseTimeout :: Microsecond
      -- ^ delay between making requests and response collection;
      -- it also means that handler will be invoked with this lag
    , ntpPollDelay       :: Microsecond
      -- ^ how often to send responses to server
    , ntpMeanSelection   :: [(Microsecond, Microsecond)] -> (Microsecond, Microsecond)
      -- ^ way to sumarize results received from different servers.
      -- this may accept list of lesser size than @length ntpServers@ in case some servers
      -- failed to respond in time, but never an empty list
    }

hoistNtpClientSettings
    :: (m () -> n ()) -> NtpClientSettings m -> NtpClientSettings n
hoistNtpClientSettings f settings =
    settings { ntpHandler = f . ntpHandler settings }

data NtpClient m = NtpClient
    { ncSocket   :: TVar Socket
    , ncState    :: TVar (Maybe [(Microsecond, Microsecond)])
    , ncClosed   :: TVar Bool
    , ncSettings :: NtpClientSettings m
    }

mkNtpClient :: MonadIO m => NtpClientSettings m -> Socket -> m (NtpClient m)
mkNtpClient ncSettings sock = liftIO $ do
    ncSocket <- newTVarIO sock
    ncState  <- newTVarIO Nothing
    ncClosed <- newTVarIO False
    return NtpClient{..}

instance Monad m => Default (NtpClientSettings m) where
    def = NtpClientSettings
        { ntpServers         = [ "ntp5.stratum2.ru"
                               , "ntp1.stratum1.ru"
                               , "clock.isc.org"
                               ]
        , ntpHandler         = \_ -> return ()
        , ntpLogName         = "ntp-cli"
        , ntpResponseTimeout = 1000000
        , ntpPollDelay       = 3000000
        , ntpMeanSelection   = \l -> let len = length l in (sortOn fst l) !! ((len - 1) `div` 2)
        }

newtype NtpStopButton m = NtpStopButton
    { pressNtpStopButton :: m ()
    }

newtype FailedToResolveHost = FailedToResolveHost String
    deriving (Show, Typeable)

instance Exception FailedToResolveHost

type NtpMonad m =
    ( MonadIO m
    , WithLogger m
    , Mockable Fork m
    , Mockable Throw m
    , Mockable Catch m
    )

log' :: NtpMonad m => NtpClientSettings m -> Severity -> Text -> m ()
log' settings severity msg =
    let logName = ntpLogName settings
    in  modifyLoggerName (<> logName) $ logMessage severity msg

log :: NtpMonad m => NtpClient m -> Severity -> Text -> m ()
log cli severity msg = do
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ log' (ncSettings cli) severity msg

handleCollectedResponses :: NtpMonad m => NtpClient m -> m ()
handleCollectedResponses cli = do
    mres <- liftIO $ readTVarIO (ncState cli)
    let selection = ntpMeanSelection (ncSettings cli)
        handler   = ntpHandler (ncSettings cli)
    case mres of
        Nothing        -> log cli Error "Protocol error: responses are not awaited"
        Just []        -> log cli Warning "No servers responded"
        Just responses -> handleE `handleAll` do
            let time = selection responses
            log cli Info $ sformat ("Evaluated clock offset "%shown%
                " mcs for request at "%shown%" mcs")
                (toMicroseconds $ fst time)
                (toMicroseconds $ snd time)
            handler time
  where
    handleE = log cli Error . sformat ("ntpMeanSelection: "%shown)

doSend :: NtpMonad m => SockAddr -> NtpClient m -> m ()
doSend addr cli = do
    sock   <- liftIO $ readTVarIO (ncSocket cli)
    packet <- encode <$> mkCliNtpPacket
    handleAll handleE . void . liftIO $ sendTo sock (LBS.toStrict packet) addr
  where
    -- just log; socket closure is handled by receiver
    handleE =
        log cli Warning . sformat ("Failed to send to "%shown%": "%shown) addr

startSend :: NtpMonad m => [SockAddr] -> NtpClient m -> m ()
startSend addrs cli = do
    let timeout = ntpResponseTimeout (ncSettings cli)
    let poll    = ntpPollDelay (ncSettings cli)
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ do
        log cli Debug "Sending requests"
        liftIO . atomically . modifyTVarS (ncState cli) $ id .= Just []
        forM_ addrs $
            \addr -> fork $ doSend addr cli
        liftIO $ threadDelay timeout

        log cli Debug "Collecting responses"
        handleCollectedResponses cli
        liftIO . atomically . modifyTVarS (ncState cli) $ id .= Nothing
        liftIO $ threadDelay (poll - timeout)

        startSend addrs cli

mkSocket :: NtpMonad m => NtpClientSettings m -> m Socket
mkSocket settings = do
    (sock, addrInfo) <- doMkSocket `catchAll` handlerE
    log' settings Info $
        sformat ("Created socket (family/addr): "%shown%"/"%shown)
                (addrFamily addrInfo) (addrAddress addrInfo)
    pure sock
  where
    doMkSocket = liftIO $ do
        let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram }
        --                          Hints        Host         Service
        serveraddrs <- getAddrInfo (Just hints) Nothing (Just $ show aNY_PORT)

        -- Couldn't be empty. Throws exception if empty.
        -- IPv6 addresses are more preffered.
        let serveraddr = preferIPv6 serveraddrs
        sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
        setSocketOption sock ReuseAddr 1
        -- See here https://hackage.haskell.org/package/network-2.6.3.1/docs/Network-Socket.html#v:socket
        setSocketOption sock IPv6Only 0
        bind sock (addrAddress serveraddr)
        pure (sock, serveraddr)
    handlerE e = do
        log' settings Warning $
            sformat ("Failed to create socket, retrying in 5 sec... (reason: "%shown%")")
            e
        liftIO $ threadDelay (5 :: Second)
        doMkSocket

handleNtpPacket :: NtpMonad m => NtpClient m -> NtpPacket -> m ()
handleNtpPacket cli packet = do
    log cli Debug $ sformat ("Got packet "%shown) packet

    clockOffset <- evalClockOffset packet

    log cli Debug $ sformat ("Received time delta "%shown%" mcs")
        (toMicroseconds clockOffset)

    late <- liftIO . atomically . modifyTVarS (ncState cli) $ do
        _Just %= ((clockOffset, ntpOriginTime packet) :)
        gets isNothing
    when late $
        log cli Warning "Response was too late"

doReceive :: NtpMonad m => NtpClient m -> m ()
doReceive cli = do
    sock <- liftIO . readTVarIO $ ncSocket cli
    forever $ do
        (received, _) <- liftIO $ recvFrom sock ntpPacketSize
        let eNtpPacket = decodeOrFail $ LBS.fromStrict received
        case eNtpPacket of
            Left  (_, _, err)    ->
                log cli Warning $ sformat ("Error while receiving time: "%shown) err
            Right (_, _, packet) ->
                handleNtpPacket cli packet

startReceive :: NtpMonad m => NtpClient m -> m ()
startReceive cli = do
    doReceive cli `catchAll` handleE
  where
    -- got error while receiving data, recreate socket
    handleE e = do
        closed <- liftIO . readTVarIO $ ncClosed cli
        unless closed $ do
            log cli Debug $ sformat ("Socket closed, recreating (reason: "%shown%")") e
            sock <- mkSocket $ ncSettings cli
            closed' <- liftIO . atomically $ do
                writeTVar (ncSocket cli) sock
                readTVar  (ncClosed cli)
            -- extra check in case socket was closed by stopping client
            -- while we recreated socket
            unless closed' $
                startReceive cli

stopNtpClient :: NtpMonad m => NtpClient m -> m ()
stopNtpClient cli = do
    log cli Info "Stopped"
    sock <- liftIO . atomically $ do
        writeTVar (ncClosed cli) True
        readTVar  (ncSocket cli)

    -- unblock receiving from socket in case no one replies
    liftIO (close sock) `catchAll` \_ -> return ()

startNtpClient :: NtpMonad m => NtpClientSettings m -> m (NtpStopButton m)
startNtpClient settings = do
    sock <- mkSocket settings
    cli <- mkNtpClient settings sock

    void . fork $ startReceive cli

    addrs <- mapM (resolveHost cli) (ntpServers settings)
    void . fork $ startSend addrs cli

    log cli Info "Launched"

    return $ NtpStopButton $ stopNtpClient cli
  where
    resolveHost cli host = do
        maddr <- liftIO $ resolveNtpHost host
        case maddr of
            Nothing   -> throw $ FailedToResolveHost host
            Just addr -> do
                log cli Info $ sformat ("Host "%shown%" is resolved: "%shown) host addr
                return addr

-- | Start client, wait for a while so that most likely it ticks once
-- and stop it.
ntpSingleShot
    :: (Mockable Delay m, NtpMonad m)
    => NtpClientSettings m -> m ()
ntpSingleShot settings = do
    stopButton <- startNtpClient settings
    delay (ntpResponseTimeout settings)
    pressNtpStopButton stopButton
