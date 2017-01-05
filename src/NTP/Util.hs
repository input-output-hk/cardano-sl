{-# LANGUAGE TypeApplications #-}

module NTP.Util
    ( datagramPacketSize
    , ntpPort
    , resolveNtpHost
    , getCurrentTime
    ) where

import           Control.Applicative       (optional)
import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Lens              (to, (^?), _head)
import           Control.Monad             (forever)
import           Control.Monad.Catch       (catchAll)
import           Control.Monad.Trans       (MonadIO (..))
import           Data.Binary               (decodeOrFail, encode)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           Data.Time.Units           (Microsecond, fromMicroseconds)
import           Data.Word                 (Word16)
import           Network.Socket            (AddrInfo (..), AddrInfoFlag (AI_ADDRCONFIG),
                                            Family (AF_INET), PortNumber, SockAddr (..),
                                            Socket, SocketOption (ReuseAddr),
                                            SocketType (Datagram), addrFamily, close,
                                            defaultHints, defaultProtocol, getAddrInfo,
                                            setSocketOption, socket)
import           Network.Socket.ByteString (recvFrom, sendManyTo)


datagramPacketSize :: Int
datagramPacketSize = 1500

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
