{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE BangPatterns          #-}

module Main where

import           Control.Applicative (empty, liftA2)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (concurrently, forConcurrently)
import           Control.Exception (throwIO)
import           Control.Exception.Safe (throwString)
import           Control.Monad (forM, forM_)

import           Data.Foldable (foldlM)
import           Data.Functor.Contravariant (contramap)
import           Data.Time.Units (Microsecond)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.TCP.Internal as TCP (encodeEndPointAddress)
import           Options.Applicative.Simple (simpleOptions)
import           System.Random (mkStdGen, randomR)

import qualified Network.Transport as NT
import           Node (Conversation (..), ConversationActions (..), Node (Node), NodeAction (..),
                       converseWith, defaultNodeEnvironment, noReceiveDelay, node,
                       simpleNodeEndPoint)
import           Node.Internal (NodeId (..))
import           Node.Message.Binary (binaryPacking)
import           Pos.Util.Trace (Severity (..), wlogTrace)

import           Bench.Network.Commons (MeasureEvent (..), Payload (..), Ping (..), Pong (..),
                                        loadLogConfig, logMeasure)
import           SenderOptions (Args (..), argsParser)

data PingState = PingState
    { _lastResetMcs    :: !Microsecond
    , _currentMessages :: !Word
    }

oneSecondMcs :: Int
oneSecondMcs = 1000000

main :: IO ()
main = do
    (Args {..}, ()) <-
        simpleOptions
            "bench-sender"
            "Sender utility for benches"
            "Use it!"
            argsParser
            empty

    loadLogConfig logsPrefix logConfig
    setLocaleEncoding utf8

    transport <- do
        transportOrError <-
            TCP.createTransport (TCP.defaultTCPAddr "127.0.0.1" "3432")
            TCP.defaultTCPParameters
        either throwIO return transportOrError

    let prngNode = mkStdGen 0
        prngWork = mkStdGen 1
        nodeIds  = [ NodeId $ TCP.encodeEndPointAddress host (show port) 0
                   | (host, port) <- recipients ]
        tasksIds = [[tid, tid + threadNum .. msgNum] | tid <- [1..threadNum]]

        action :: IO ()
        action = do
            startTime <- round . (* 1000000) <$> getPOSIXTime

            -- TODO: is it good idea to start (recipients number * thread number) threads?
            let pingWorkers = liftA2 (pingSender prngWork payloadBound startTime msgRate)
                                     tasksIds
                                     (zip [0, msgNum..] nodeIds)

            node logTrace (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) prngNode binaryPacking () defaultNodeEnvironment $ \node' ->
                NodeAction (const []) $ \converse -> () <$ do
                    drones <- forM nodeIds (startDrone node')
                    forConcurrently pingWorkers ($ converse) `concurrently` do
                        threadDelay (duration * 1000000)
                        forM_ drones stopDrone

    action

  where

    logTrace = wlogTrace "sender"
    logTrace' = contramap ((,) Info) logTrace

    pingSender gen payloadBound startTimeMcs msgRate msgIds (msgStartId, peerId) converse =
        foldlM (pingSenderOnce payloadBound msgRate msgStartId peerId converse)
               (gen, PingState startTimeMcs 0)
               msgIds

    pingSenderOnce payloadBound msgRate msgStartId peerId converse (gen, !pingState) msgId = do
        let sMsgId = msgStartId + msgId
            (i, gen') = randomR (0, payloadBound) gen
            payload = Payload i
        logMeasure logTrace' PingSent sMsgId payload
        converseWith converse peerId $ \_ -> Conversation $ \cactions -> do
            send cactions (Ping sMsgId payload)
            recv cactions maxBound >>= \case
                Just (Pong _ _) -> pure ()
                _ -> throwString "Expected a pong"
            pure ()
        curTime <- round . (* 1000000) <$> getPOSIXTime
        let fromLastReset = curTime - fromIntegral (_lastResetMcs pingState)
        pingState' <-
            if fromLastReset >= oneSecondMcs
            then pure (PingState (fromIntegral curTime) 0)
            else if _currentMessages pingState >= msgRate
                then do
                    let waitToNextSecond = oneSecondMcs - fromLastReset
                    threadDelay waitToNextSecond
                    freshTime <- round .(* 1000000) <$> getPOSIXTime
                    pure $ PingState freshTime 0
                else pure $ pingState { _currentMessages = _currentMessages pingState + 1 }
        pure (gen', pingState')

    startDrone
        :: Node
        -> NodeId
        -> IO NT.Connection
    startDrone (Node _ endPoint _) (NodeId peer) = do
        connOrErr <- NT.connect endPoint peer NT.ReliableOrdered (NT.ConnectHints Nothing)
        either throwIO return connOrErr

    stopDrone = NT.close
