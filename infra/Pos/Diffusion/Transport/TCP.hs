-- | Convenient TCP transport acquire and release with common configuration
-- options.

module Pos.Diffusion.Transport.TCP
    ( bracketTransportTCP
    ) where

import           Universum hiding (bracket)

import           Control.Exception (bracket, throwIO)
import           Data.Time.Units (Microsecond)
import           Formatting (sformat, shown, (%))

import           Network.QDisc.Fair (fairQDisc)
import qualified Network.Transport as NT
import qualified Network.Transport.TCP as TCP
import           Pos.Util.Trace (Trace, traceWith)

-- | Bracket a TCP transport with
--
--   - Given connection timeout in us
--   - Given address (possibly unaddressable)
--   - A fair QDisc
--   - Check the peer host against resolved host (prevents easy denial-of-service)
--   - Do not crash the server if 'accept' fails; instead, use the given
--     'Trace' to log the reason and continue trying to accept new connections
bracketTransportTCP
    :: Trace IO Text
    -> Microsecond
    -> TCP.TCPAddr
    -> (NT.Transport -> IO a)
    -> IO a
bracketTransportTCP logTrace connectionTimeout tcpAddr k = bracket
    (createTransportTCP logTrace connectionTimeout tcpAddr)
    NT.closeTransport
    k

createTransportTCP
    :: Trace IO Text -- ^ Whenever there's an error accepting a new connection.
    -> Microsecond   -- ^ Connection timeout
    -> TCP.TCPAddr
    -> IO NT.Transport
createTransportTCP logTrace connectionTimeout addrInfo = do
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral connectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = True
             , TCP.tcpServerExceptionHandler = \e ->
                   traceWith logTrace (sformat ("Exception in tcp server: " % shown) e)
             })
    TCP.createTransport addrInfo tcpParams >>= either throwIO pure
