-- | Convenient TCP transport acquire and release with common configuration
-- options.

module Pos.Infra.Diffusion.Transport.TCP
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
--   - Optionally check the peer host against resolved host, which prevents easy
--     denial-of-service attacks
--   - Do not crash the server if 'accept' fails; instead, use the given
--     'Trace' to log the reason and continue trying to accept new connections
bracketTransportTCP
    :: Trace IO Text
    -> Microsecond
    -> TCP.TCPAddr
    -> Bool
    -> (NT.Transport -> IO a)
    -> IO a
bracketTransportTCP logTrace connectionTimeout tcpAddr checkPeerHost k = bracket
    (createTransportTCP logTrace connectionTimeout tcpAddr checkPeerHost)
    NT.closeTransport
    k

createTransportTCP
    :: Trace IO Text -- ^ Whenever there's an error accepting a new connection.
    -> Microsecond   -- ^ Connection timeout
    -> TCP.TCPAddr
    -> Bool          -- ^ Whether to perform the TCP peer address consistency.
    -> IO NT.Transport
createTransportTCP logTrace connectionTimeout addrInfo checkPeerHost = do
    unless checkPeerHost $ do
      traceWith logTrace "DANGER: peer host address check disabled!  Node is vulnerable to DoS attacks."
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral connectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = checkPeerHost
             , TCP.tcpServerExceptionHandler = \e ->
                   traceWith logTrace (sformat ("Exception in tcp server: " % shown) e)
             })
    TCP.createTransport addrInfo tcpParams >>= either throwIO pure
