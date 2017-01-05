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
import           Control.TimeWarp.Timed     (for)
import           Data.Binary
import           Data.String                (fromString)
import           Data.Time.Units            (Microsecond, fromMicroseconds)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Message.Message            (BinaryP (..))
import           Mockable.Concurrent        (wait)
import           Mockable.Production
import           Network.Transport.Abstract (newEndPoint)
import           Network.Transport.Concrete (concrete)
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
deriving instance Generic Ping
deriving instance Show Ping
instance Binary Ping

-- | Type for messages from the listeners to the workers.
data Pong = Pong
deriving instance Generic Pong
deriving instance Show Pong
instance Binary Pong

type Packing = BinaryP

workers :: NodeId -> StdGen -> [NodeId] -> [Worker Packing Production]
workers anId generator peerIds = [pingWorker generator]
    where
    pingWorker :: StdGen -> SendActions Packing Production -> Production ()
    pingWorker gen sendActions = loop gen
        where
        loop :: StdGen -> Production ()
        loop g = do
            let (i, gen') = randomR (0,1000000) g
                us = fromMicroseconds i :: Microsecond
            wait $ for us
            let pong :: NodeId -> ConversationActions Void Pong Production -> Production ()
                pong peerId cactions = do
                    liftIO . putStrLn $ show anId ++ " sent PING to " ++ show peerId
                    received <- recv cactions
                    case received of
                        Just Pong -> liftIO . putStrLn $ show anId ++ " heard PONG from " ++ show peerId
                        Nothing -> error "Unexpected end of input"
            forM_ peerIds $ \peerId -> withConnectionTo sendActions peerId (fromString "ping") (pong peerId)
            loop gen'

listeners :: NodeId -> [Listener Packing Production]
listeners anId = [Listener (fromString "ping") pongWorker]
    where
    pongWorker :: ListenerAction Packing Production
    pongWorker = ListenerActionConversation $ \peerId (cactions :: ConversationActions Pong Void Production) -> do
        liftIO . putStrLn $ show anId ++  " heard PING from " ++ show peerId
        send cactions Pong
        liftIO . putStrLn $ show anId ++ " sent PONG to " ++ show peerId

main :: IO ()
main = runProduction $ do

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
