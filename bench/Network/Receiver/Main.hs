{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Control.Applicative        (empty)
import qualified Control.Exception.Lifted   as Exception
import           Control.Monad              (unless)

import           Data.Time.Units            (Second)
import           GHC.IO.Encoding            (setLocaleEncoding, utf8)
import qualified Network.Transport.TCP      as TCP
import           Options.Applicative.Simple (simpleOptions)
import           Serokell.Util.Concurrent   (threadDelay)
import           System.Random              (mkStdGen)
import           System.Wlog                (LoggerNameBox, usingLoggerName)

import           Mockable.Class             (Mockable (..))
import           Mockable.Exception         (Catch (..))

import           Bench.Network.Commons      (MeasureEvent (..), Ping (..), Pong (..),
                                             loadLogConfig, logMeasure)
import           Network.Transport.Abstract (newEndPoint)
import           Network.Transport.Concrete (concrete)
import           Node                       (Listener (..), ListenerAction (..), sendTo,
                                             startNode, stopNode)
import           Message.Message            (BinaryP (..))
import           ReceiverOptions            (Args (..), argsParser)

instance Mockable Catch (LoggerNameBox IO) where
    liftMockable (Catch action handler) = action `Exception.catch` handler

type Header = ()

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

    Right transport_ <- TCP.createTransport ("0.0.0.0") (show port)
        TCP.defaultTCPParameters
    let transport = concrete transport_

    let prng = mkStdGen 0

    usingLoggerName "receiver" $ do
        Right endPoint <- newEndPoint transport
        receiverNode <- startNode @Header endPoint prng BinaryP [] Nothing
            [Listener "ping" $ pingListener noPong]

        threadDelay (fromIntegral duration :: Second)
        stopNode receiverNode
  where
    pingListener noPong =
        -- TODO: `ListenerActionConversation` is not supported in such context
        -- why? how should it be used?
        ListenerActionOneMsg $ \peerId sendActions (Ping mid payload) -> do
            logMeasure PingReceived mid payload
            unless noPong $ do
                logMeasure PongSent mid payload
                sendTo sendActions peerId "pong" () $ Pong mid payload
