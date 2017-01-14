{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Control.Applicative        (empty, liftA2)
import           Control.Lens               (makeLenses, (+=))
import           Control.Monad              (forM, forM_)
import           Control.Monad.Random       (evalRandT, getRandomR)
import           Control.Monad.State        (evalStateT, get, put)
import           Control.Monad.Trans        (MonadIO (liftIO), lift)

import           Data.Time.Units            (Microsecond, Second, convertUnit)
import           GHC.IO.Encoding            (setLocaleEncoding, utf8)
import qualified Network.Transport.TCP      as TCP
import           Options.Applicative.Simple (simpleOptions)
import           Serokell.Util.Concurrent   (threadDelay)
import           System.Random              (mkStdGen)
import           System.Wlog                (usingLoggerName)

import           Mockable                   (fork, realTime, runProduction)
import qualified Network.Transport.Abstract as NT
import           Network.Transport.Concrete (concrete)
import           Node                       (ListenerAction (..), NodeAction (..), node,
                                             nodeEndPoint, sendTo)
import           Node.Internal              (NodeId (..))
import           Node.Message               (BinaryP (..))


import           Bench.Network.Commons      (MeasureEvent (..), Payload (..), Ping (..),
                                             Pong (..), loadLogConfig, logMeasure)
import           SenderOptions              (Args (..), argsParser)

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

    Right transport_ <- TCP.createTransport "0.0.0.0" "127.0.0.1" "3432" TCP.defaultTCPParameters
    let transport = concrete transport_

    let prngNode = mkStdGen 0
    let prngWork = mkStdGen 1
    let nodeIds  = [ NodeId $ TCP.encodeEndPointAddress host (show port) 0
                   | (host, port) <- recipients ]
    let tasksIds = [[tid, tid + threadNum .. msgNum] | tid <- [1..threadNum]]

    runProduction $ usingLoggerName "sender" $ do
        startTime <- realTime

        -- TODO: is it good idea to start (recipients number * thread number) threads?
        let pingWorkers = liftA2 (pingSender prngWork payloadBound startTime msgRate)
                                 tasksIds
                                 (zip [0, msgNum..] nodeIds)
        node transport prngNode BinaryP $ \node' ->
            pure $ NodeAction [pongListener] $ \sactions -> do
                let endPoint = nodeEndPoint node'
                drones <- forM nodeIds (startDrone endPoint)
                _ <- forM pingWorkers (fork . flip ($) sactions)
                threadDelay (fromIntegral duration :: Second)
                forM_ drones stopDrone
  where
    pongListener = ListenerActionOneMsg $ \_ _ (Pong mid payload) ->
        logMeasure PongReceived mid payload

    pingSender gen payloadBound startTimeMcs msgRate msgIds (msgStartId, peerId) sendActions =
        (`evalRandT` gen) . (`evalStateT` PingState startTimeMcs 0) . forM_ msgIds $ \msgId -> do
            let sMsgId = msgStartId + msgId
            payload   <- liftIO $ Payload <$> getRandomR (0, payloadBound)
            lift . lift $ logMeasure PingSent sMsgId payload
            -- TODO: better to use `connect` + `send`,
            -- but `connect` is not implemented yet
            lift . lift $ sendTo sendActions peerId $ Ping sMsgId payload

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

    startDrone endPoint (NodeId addr) = do
        Right conn <- NT.connect endPoint addr NT.ReliableOrdered (NT.ConnectHints Nothing)
        pure conn

    stopDrone = NT.close
