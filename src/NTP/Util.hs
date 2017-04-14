{-# LANGUAGE TypeApplications #-}

module NTP.Util
    ( ntpPort
    , resolveNtpHost
    , getCurrentTime
    , preferIPv6
    ) where

import           Control.Lens          (to, (^?), _head)
import           Control.Monad.Catch   (catchAll)
import           Control.Monad.Trans   (MonadIO (..))
import           Data.List             (sortOn)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units       (Microsecond, fromMicroseconds)
import           Network.Socket        (AddrInfo, AddrInfoFlag (AI_ADDRCONFIG),
                                        Family (AF_INET, AF_INET6), PortNumber (..),
                                        SockAddr (..), SocketType (Datagram), addrAddress,
                                        addrFamily, addrFlags, addrSocketType,
                                        defaultHints, getAddrInfo)


ntpPort :: PortNumber
ntpPort = 123

resolveHost :: String -> IO (Maybe SockAddr)
resolveHost host = do
    let hints = defaultHints
            { addrSocketType = Datagram
            , addrFlags = [AI_ADDRCONFIG]  -- since we use AF_INET family
            }
    addrInfos <- getAddrInfo (Just hints) (Just host) Nothing
                    `catchAll` \_ -> return []

    -- one address is enough
    return $ addrInfos ^? _head . to addrAddress

replacePort :: SockAddr -> PortNumber -> SockAddr
replacePort (SockAddrInet  _ host)            port = SockAddrInet  port host
replacePort (SockAddrInet6 _ flow host scope) port = SockAddrInet6 port flow host scope
replacePort sockAddr                          _    = sockAddr

resolveNtpHost :: String -> IO (Maybe SockAddr)
resolveNtpHost host = do
    addr <- resolveHost host
    return $ flip replacePort ntpPort <$> addr

getCurrentTime :: MonadIO m => m Microsecond
getCurrentTime = liftIO $ fromMicroseconds . round . ( * 1000000) <$> getPOSIXTime

preferIPv6 :: [AddrInfo] -> AddrInfo
preferIPv6 =
    head .
    reverse .
    sortOn addrFamily .
    filter (\a -> addrFamily a == AF_INET6 || addrFamily a == AF_INET)
