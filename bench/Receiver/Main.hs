{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Control.Applicative        (empty)
import           Control.Monad              (unless)

import           Data.Time.Units            (Second)
import           GHC.IO.Encoding            (setLocaleEncoding, utf8)
import           Options.Applicative.Simple (simpleOptions)
import           Serokell.Util.Concurrent   (threadDelay)
import           System.Random              (mkStdGen)
import           System.Wlog                (usingLoggerName)

import           Mockable                   (Production (runProduction))

import           Bench.Network.Commons      (MeasureEvent (..), Ping (..), Pong (..),
                                             loadLogConfig, logMeasure)
import qualified Network.Transport.TCP      as TCP
import           Network.Transport.Concrete (concrete)
import           Node                       (ListenerAction (..), NodeAction (..), node,
                                             sendTo)
import           Node.Message               (BinaryP (..))
import           ReceiverOptions            (Args (..), argsParser)

main :: IO ()
main = do
    (Args {..}, ()) <-
        simpleOptions
            "bench-receiver"
            "Server utility for benches"
            "Use it!"
            argsParser
            empty

    loadLogConfig logsPrefix logConfig
    setLocaleEncoding utf8

    Right transport_ <- TCP.createTransport "0.0.0.0" "127.0.0.1" (show port)
        TCP.defaultTCPParameters
    let transport = concrete transport_

    let prng = mkStdGen 0

    runProduction $ usingLoggerName "receiver" $ do
        node transport prng BinaryP () $ \_ ->
            NodeAction [pingListener noPong] $ \_ -> do
                threadDelay (fromIntegral duration :: Second)
  where
    pingListener noPong =
        -- TODO: `ListenerActionConversation` is not supported in such context
        -- why? how should it be used?
        ListenerActionOneMsg $ \_ peerId sendActions (Ping mid payload) -> do
            logMeasure PingReceived mid payload
            unless noPong $ do
                logMeasure PongSent mid payload
                sendTo sendActions peerId $ Pong mid payload
