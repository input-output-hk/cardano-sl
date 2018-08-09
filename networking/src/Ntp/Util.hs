{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ntp.Util
    ( ntpPort
    , resolveNtpHost
    , getCurrentTime
    , selectIPv6
    , selectIPv4

    , createAndBindSock
    , udpLocalAddresses

    , withSocketsDoLifted
    ) where

import           Control.Exception (IOException, catch)
import           Control.Monad.Trans (MonadIO (..))
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Data.List (find, sortOn)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Network.Socket (AddrInfo, AddrInfoFlag (AI_ADDRCONFIG, AI_PASSIVE),
                                 Family (AF_INET, AF_INET6), PortNumber (..), SockAddr (..), Socket,
                                 SocketOption (ReuseAddr), SocketType (Datagram), aNY_PORT,
                                 addrAddress, addrFamily, addrFlags, addrSocketType, bind,
                                 defaultHints, defaultProtocol, getAddrInfo, setSocketOption,
                                 socket)

ntpPort :: PortNumber
ntpPort = 123

resolveHost :: String -> (Bool, Bool) -> IO (Maybe SockAddr)
resolveHost host (hasIPv4, hasIPv6) = do
    let hints = defaultHints
            { addrSocketType = Datagram
            , addrFlags = [AI_ADDRCONFIG]  -- since we use AF_INET family
            }
    -- TBD why catch here? Why not let 'resolveHost' throw the exception?
    addrInfos <- getAddrInfo (Just hints) (Just host) Nothing
                    `catch` (\(_ :: IOException) -> return [])

    -- one address is enough
    pure $
        if null addrInfos then Nothing
        else if hasIPv6 && hasIPv4 then Just $ addrAddress $ preferIPv4 addrInfos
        else fmap addrAddress $ if hasIPv4 then selectIPv4 addrInfos else selectIPv6 addrInfos

replacePort :: SockAddr -> PortNumber -> SockAddr
replacePort (SockAddrInet  _ host)            port = SockAddrInet  port host
replacePort (SockAddrInet6 _ flow host scope) port = SockAddrInet6 port flow host scope
replacePort sockAddr                          _    = sockAddr

resolveNtpHost :: String -> (Bool, Bool) -> IO (Maybe SockAddr)
resolveNtpHost host whichSockets = do
    addr <- resolveHost host whichSockets
    return $ flip replacePort ntpPort <$> addr

getCurrentTime :: MonadIO m => m Microsecond
getCurrentTime = liftIO $ fromMicroseconds . round . ( * 1000000) <$> getPOSIXTime

preferIPv4 :: [AddrInfo] -> AddrInfo
preferIPv4 =
    head .
    sortOn addrFamily .
    filter (\a -> addrFamily a == AF_INET6 || addrFamily a == AF_INET)

selectIPv6 :: [AddrInfo] -> Maybe AddrInfo
selectIPv6 = find (\a -> addrFamily a == AF_INET6)

selectIPv4 :: [AddrInfo] -> Maybe AddrInfo
selectIPv4 = find (\a -> addrFamily a == AF_INET)

createAndBindSock
    :: ([AddrInfo] -> Maybe AddrInfo)
    -> [AddrInfo]
    -> IO (Maybe (Socket, AddrInfo))
createAndBindSock addrSelector serveraddrs =
    traverse createDo (addrSelector serveraddrs)
  where
    createDo serveraddr = do
        sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress serveraddr)
        pure (sock, serveraddr)

udpLocalAddresses :: IO [AddrInfo]
udpLocalAddresses = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram }
    --                          Hints        Host         Service
    getAddrInfo (Just hints) Nothing (Just $ show aNY_PORT)

-- | Lifted version of `withSocketsDo`.
withSocketsDoLifted :: MonadBaseControl IO m => m a -> m a
withSocketsDoLifted action =
    restoreM =<< (liftBaseWith $ \runInIO -> runInIO action)
