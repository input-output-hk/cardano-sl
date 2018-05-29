{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE LambdaCase            #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Exception (throwIO)
import           Control.Exception.Safe (throwString)
import           Control.Applicative (empty)
import           Control.Monad (unless)

--import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text)
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Options.Applicative.Simple (simpleOptions)
import           System.Random (mkStdGen)

import           Bench.Network.Commons (MeasureEvent (..), Ping (..), Pong (..),
                                        logMeasure)
import qualified Network.Transport.TCP as TCP
import           Node (ConversationActions (..), Listener (..), NodeAction (..),
                       defaultNodeEnvironment, noReceiveDelay, node, simpleNodeEndPoint)
import           Node.Message.Binary (binaryPacking)
import           ReceiverOptions (Args (..), argsParser)
import           Pos.Util.Trace (Trace, TraceIO, logTrace, logInfo)
import qualified Pos.Util.Log as Log

main :: IO ()
main = do
    (Args {..}, ()) <-
        simpleOptions
            "bench-receiver"
            "Server utility for benches"
            "Use it!"
            argsParser
            empty

    Log.loadLogConfig logsPrefix logConfig
    setLocaleEncoding utf8

    transport <- do
        transportOrError <-
            TCP.createTransport (TCP.defaultTCPAddr "127.0.0.1" (show port))
            TCP.defaultTCPParameters
        either throwIO return transportOrError

    let prng = mkStdGen 0

    node logTrace' (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) prng binaryPacking () defaultNodeEnvironment $ \_ ->
        NodeAction (const [pingListener noPong]) $ \_ -> do
            threadDelay (duration * 1000000)
  where

    logTrace' :: TraceIO
    logTrace' = logTrace "receiver"

    logInfo_ :: Trace IO Text
    logInfo_ = logInfo logTrace'

    pingListener noPong =
        Listener $ \_ _ cactions -> do
            (mid, payload) <- recv cactions maxBound >>= \case
                Just (Ping mid payload) -> return (mid, payload)
                _ -> throwString "Expected a ping"
            logMeasure logInfo_ PingReceived mid payload
            unless noPong $ do
                logMeasure logInfo_ PongSent mid payload
                send cactions (Pong mid payload)
