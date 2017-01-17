{-# LANGUAGE TypeApplications #-}

module NTP.Util
    ( ntpPort
    , resolveNtpHost
    , getCurrentTime
    ) where

import           Control.Lens          (to, (^?), _head)
import           Control.Monad.Catch   (catchAll)
import           Control.Monad.Trans   (MonadIO (..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units       (Microsecond, fromMicroseconds)
import           Network.Socket        (AddrInfoFlag (AI_ADDRCONFIG), PortNumber (..),
                                        SockAddr (..), SocketType (Datagram), addrAddress,
                                        addrFlags, addrSocketType, defaultHints,
                                        getAddrInfo)


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
