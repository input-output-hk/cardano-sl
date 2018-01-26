{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE LambdaCase            #-}

module Main where

import           Control.Applicative (empty, liftA2)
import           Control.Exception.Safe (throwString, throwM)
import           Control.Lens (makeLenses, (+=))
import           Control.Monad (forM, forM_)
import           Control.Monad.Random (evalRandT, getRandomR)
import           Control.Monad.State (evalStateT, get, put)
import           Control.Monad.Trans (MonadIO (liftIO), lift)

import           Data.Time.Units (Microsecond, Second, convertUnit)
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.TCP.Internal as TCP (encodeEndPointAddress)
import           Options.Applicative.Simple (simpleOptions)
import           Serokell.Util.Concurrent (threadDelay)
import           System.Random (mkStdGen)
import           System.Wlog (LoggerNameBox, usingLoggerName)

import           Mockable (Production, concurrently, delay, forConcurrently, realTime,
                           runProduction)
import qualified Network.Transport.Abstract as NT
import           Network.Transport.Concrete (concrete)
import           Node (Conversation (..), ConversationActions (..), Node (Node), NodeAction (..),
                       converseWith, defaultNodeEnvironment, noReceiveDelay, node,
                       simpleNodeEndPoint)
import           Node.Internal (NodeId (..))
import           Node.Message.Binary (binaryPacking)


import           Bench.Network.Commons (MeasureEvent (..), Payload (..), Ping (..), Pong (..),
                                        loadLogConfig, logMeasure)
import           SenderOptions (Args (..), argsParser)

data PingState = PingState
    { _lastResetMcs    :: !Microsecond
    , _currentMessages :: !Word
    }

makeLenses ''PingState

oneSecondMcs :: Microsecond
oneSecondMcs = convertUnit @Second 1

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

    transport_ <- do
        transportOrError <-
            TCP.createTransport (TCP.defaultTCPAddr "127.0.0.1" "3432")
            TCP.defaultTCPParameters
        either throwM return transportOrError
    let transport = concrete transport_

    let prngNode = mkStdGen 0
    let prngWork = mkStdGen 1
    let nodeIds  = [ NodeId $ TCP.encodeEndPointAddress host (show port) 0
                   | (host, port) <- recipients ]
    let tasksIds = [[tid, tid + threadNum .. msgNum] | tid <- [1..threadNum]]

    let action :: LoggerNameBox Production ()
        action = do
            startTime <- realTime

            -- TODO: is it good idea to start (recipients number * thread number) threads?
            let pingWorkers = liftA2 (pingSender prngWork payloadBound startTime msgRate)
                                     tasksIds
                                     (zip [0, msgNum..] nodeIds)

            node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) prngNode binaryPacking () defaultNodeEnvironment $ \node' ->
                NodeAction (const []) $ \converse -> () <$ do
                    drones <- forM nodeIds (startDrone node')
                    forConcurrently pingWorkers ($ converse) `concurrently` do
                        delay (fromIntegral duration :: Second)
                        forM_ drones stopDrone

    runProduction $ usingLoggerName "sender" $ action
  where

    pingSender gen payloadBound startTimeMcs msgRate msgIds (msgStartId, peerId) converse =
        (`evalRandT` gen) . (`evalStateT` PingState startTimeMcs 0) . forM_ msgIds $ \msgId -> do
            let sMsgId = msgStartId + msgId
            payload   <- liftIO $ Payload <$> getRandomR (0, payloadBound)
            lift . lift $ logMeasure PingSent sMsgId payload
            -- TODO: better to use `connect` + `send`,
            -- but `connect` is not implemented yet
            lift . lift $ converseWith converse peerId $
                \_ -> Conversation $ \cactions -> do
                    send cactions (Ping sMsgId payload)
                    recv cactions maxBound >>= \case
                        Just (Pong _ _) -> return ()
                        _ -> throwString "Expected a pong"
                    return ()

            PingState{..}    <- get
            curTime          <- realTime
            let fromLastReset = curTime - _lastResetMcs

            if fromLastReset >= oneSecondMcs then
                put $ PingState curTime 0
            else if _currentMessages >= msgRate then do
                let waitToNextSecond = oneSecondMcs - fromLastReset
                threadDelay waitToNextSecond
                freshTime <- realTime
                put $ PingState freshTime 0
            else
                currentMessages += 1

    startDrone
        :: Node (LoggerNameBox Production)
        -> NodeId
        -> LoggerNameBox Production (NT.Connection (LoggerNameBox Production))
    startDrone (Node _ endPoint _) (NodeId peer) = do
        connOrErr <- NT.connect endPoint peer NT.ReliableOrdered (NT.ConnectHints Nothing)
        either throwM return connOrErr

    stopDrone = NT.close
