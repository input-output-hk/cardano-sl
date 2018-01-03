-- | Convenient TCP transport acquire and release with common configuration
-- options.

module Pos.Diffusion.Transport.TCP
    ( bracketTransportTCP
    ) where

import           Universum

import           Formatting (sformat, shown, (%))
import           System.Wlog (WithLogger, logError, usingLoggerName, askLoggerName)

import           Network.QDisc.Fair (fairQDisc)
import qualified Network.Transport as NT (closeTransport)
import           Network.Transport.Abstract (Transport)
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP as TCP

import           Pos.Configuration (HasNodeConfiguration, networkConnectionTimeout)

bracketTransportTCP
    :: ( HasNodeConfiguration
       , MonadIO m
       , MonadIO n
       , MonadThrow m
       , MonadMask m
       , WithLogger m
       )
    => TCP.TCPAddr
    -> (Transport n -> m a)
    -> m a
bracketTransportTCP tcpAddr k = bracket (createTransportTCP tcpAddr) snd (k . fst)

createTransportTCP
    :: ( HasNodeConfiguration
       , MonadIO n
       , MonadIO m
       , WithLogger m
       , MonadThrow m
       )
    => TCP.TCPAddr
    -> m (Transport n, m ())
createTransportTCP addrInfo = do
    loggerName <- askLoggerName
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral networkConnectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = True
             , TCP.tcpServerExceptionHandler = \e ->
                     usingLoggerName (loggerName <> "transport") $
                         logError $ sformat ("Exception in tcp server: " % shown) e
             })
    transportE <- liftIO $ TCP.createTransport addrInfo tcpParams
    case transportE of
        Left e -> do
            logError $ sformat ("Error creating TCP transport: " % shown) e
            throwM e
        Right transport -> return (concrete transport, liftIO $ NT.closeTransport transport)
