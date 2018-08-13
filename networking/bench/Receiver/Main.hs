{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Control.Applicative (empty)
import           Control.Concurrent (threadDelay)
import           Control.Exception (throwIO)
import           Control.Exception.Safe (throwString)
import           Control.Monad (unless)

import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Options.Applicative.Simple (simpleOptions)
import           System.Random (mkStdGen)

import           Bench.Network.Commons (MeasureEvent (..), Ping (..), Pong (..),
                     logMeasure)
import qualified Network.Transport.TCP as TCP
import           Node (ConversationActions (..), Listener (..), NodeAction (..),
                     defaultNodeEnvironment, noReceiveDelay, node,
                     simpleNodeEndPoint)
import           Node.Message.Binary (binaryPacking)
import qualified Pos.Util.Log as Log
import           Pos.Util.LoggerConfig
import           Pos.Util.Trace (Severity (..), Trace, wlogTrace)
import           Pos.Util.Trace.Named (appendName, setupLogging)
import           ReceiverOptions (Args (..), argsParser)

main :: IO ()
main = do
    (Args {..}, ()) <-
        simpleOptions
            "bench-receiver"
            "Server utility for benches"
            "Use it!"
            argsParser
            empty

    lc1 <- case logConfig of
              Nothing  -> return $ defaultInteractiveConfiguration Log.Debug
              Just lc0 -> parseLoggerConfig lc0
    lc <- setLogPrefix logsPrefix lc1
    logTrace <- setupLogging lc "bench-receiver"

    setLocaleEncoding utf8

    transport <- do
        transportOrError <-
            TCP.createTransport (TCP.defaultTCPAddr "127.0.0.1" (show port))
            TCP.defaultTCPParameters
        either throwIO return transportOrError

    let prng = mkStdGen 0

    node (appendName "node" logTrace) (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) prng binaryPacking () defaultNodeEnvironment $ \_ ->
        NodeAction (const [pingListener logTrace noPong]) $ \_ -> do
            threadDelay (duration * 1000000)
  where

    pingListener logTrace noPong =
        Listener $ \_ _ cactions -> do
            (mid, payload) <- recv cactions maxBound >>= \case
                Just (Ping mid payload) -> return (mid, payload)
                _ -> throwString "Expected a ping"
            logMeasure (appendName "ping" logTrace) PingReceived mid payload
            unless noPong $ do
                logMeasure (appendName "pong" logTrace) PongSent mid payload
                send cactions (Pong mid payload)
