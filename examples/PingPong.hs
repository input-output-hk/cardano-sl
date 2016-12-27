{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Monad              (forM_)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Binary
import           Data.String                (fromString)
<<<<<<< a44701cf6ab26ce4d5fab2776c5d079a00076529
import           Message.Message            (BinaryP (..))
import           Mockable.Concurrent        (delay)
=======
import           Mockable.Concurrent        (wait)
import           Control.TimeWarp.Timed     (for)
import           Data.Time.Units            (fromMicroseconds, Microsecond)
>>>>>>> [CSL-447] wait instead of delay
import           Mockable.Production
import           Network.Transport.Abstract (newEndPoint)
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.InMemory as InMemory
import qualified Network.Transport.TCP      as TCP
import           Node
import           System.Random

-- Sending a message which encodes to "" is problematic!
-- The receiver can't distinuish this from the case in which the sender sent
-- nothing at all.
-- So we give custom Ping and Pong types with non-generic Binary instances.
--
-- TBD should we fix this in network-transport? Maybe every chunk is prefixed
-- by a byte giving its length? Wasteful I guess but maybe not a problem.

-- | Type for messages from the workers to the listeners.
data Ping = Ping
deriving instance Show Ping
instance Binary Ping where
    put _ = putWord8 (fromIntegral 1)
    get = do
        w <- getWord8
        if w == fromIntegral 1
        then pure Ping
        else fail "no parse ping"

-- | Type for messages from the listeners to the workers.
data Pong = Pong
deriving instance Show Pong
instance Binary Pong where
    put _ = putWord8 (fromIntegral 1)
    get = do
        w <- getWord8
        if w == fromIntegral 1
        then pure Pong
        else fail "no parse pong"

type Packing = BinaryP

workers :: NodeId -> StdGen -> [NodeId] -> [Worker Packing Production]
workers id gen peerIds = [pingWorker gen]
    where
    pingWorker :: StdGen -> SendActions Packing Production -> Production ()
    pingWorker gen sendActions = loop gen
        where
        loop :: StdGen -> Production ()
        loop gen = do
            let (i, gen') = randomR (0,1000000) gen
<<<<<<< a44701cf6ab26ce4d5fab2776c5d079a00076529
            delay i
            let pong :: NodeId -> ConversationActions Ping Pong Production -> Production ()
=======
                us = fromMicroseconds i :: Microsecond
            wait $ for us
            let pong :: NodeId -> ConversationActions Header Ping Pong Production -> Production ()
>>>>>>> [CSL-447] wait instead of delay
                pong peerId cactions = do
                    liftIO . putStrLn $ show id ++ " sent PING to " ++ show peerId
                    received <- recv cactions
                    case received of
                        Just Pong -> liftIO . putStrLn $ show id ++ " heard PONG from " ++ show peerId
                        Nothing -> error "Unexpected end of input"
            forM_ peerIds $ \peerId -> withConnectionTo sendActions peerId (fromString "ping") (pong peerId)
            loop gen'

listeners :: NodeId -> [Listener Packing Production]
listeners id = [Listener (fromString "ping") pongWorker]
    where
    pongWorker :: ListenerAction Packing Production
    pongWorker = ListenerActionConversation $ \peerId (cactions :: ConversationActions Pong Ping Production) -> do
        liftIO . putStrLn $ show id ++  " heard PING from " ++ show peerId
        send cactions Pong
        liftIO . putStrLn $ show id ++ " sent PONG to " ++ show peerId

main = runProduction $ do

    --transport_ <- InMemory.createTransport
    Right transport_ <- liftIO $ TCP.createTransport ("127.0.0.1") ("10128") TCP.defaultTCPParameters
    let transport = concrete transport_
    Right endpoint1 <- newEndPoint transport
    Right endpoint2 <- newEndPoint transport

    let prng1 = mkStdGen 0
    let prng2 = mkStdGen 1
    let prng3 = mkStdGen 2
    let prng4 = mkStdGen 3

    liftIO . putStrLn $ "Starting nodes"
    rec { node1 <- startNode endpoint1 prng1 BinaryP (workers nodeId1 prng2 [nodeId2])
            (listeners nodeId1)
        ; node2 <- startNode endpoint2 prng3 BinaryP (workers nodeId2 prng4 [nodeId1])
            (listeners nodeId2)
        ; let nodeId1 = nodeId node1
        ; let nodeId2 = nodeId node2
        }

    liftIO . putStrLn $ "Hit return to stop"
    _ <- liftIO getChar

    liftIO . putStrLn $ "Stopping node"
    stopNode node1
    stopNode node2
