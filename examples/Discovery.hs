{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Monad                        (forM, forM_, when)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Binary
import qualified Data.ByteString.Char8                as B8
import qualified Data.Set                             as S
import           Data.Time.Units                      (Microsecond, fromMicroseconds)
import           Data.Void                            (Void)
import           GHC.Generics                         (Generic)
import           Mockable.Concurrent                  (ThreadId, delay, fork, killThread)
import           Mockable.Exception                   (finally)
import           Mockable.Production
import           Network.Discovery.Abstract
import qualified Network.Discovery.Transport.Kademlia as K
import           Network.Transport.Abstract           (Transport (..))
import           Network.Transport.Concrete           (concrete)
import qualified Network.Transport.TCP                as TCP
import           Node
import           Node.Message                         (BinaryP (..))
import           System.Environment                   (getArgs)
import           System.Random

data Pong = Pong
deriving instance Generic Pong
deriving instance Show Pong
instance Binary Pong where

type Packing = BinaryP

worker :: NodeId -> StdGen -> NetworkDiscovery K.KademliaDiscoveryErrorCode Production -> Worker Packing Production
worker anId generator discovery = pingWorker generator
    where
    pingWorker :: StdGen -> SendActions Packing Production -> Production ()
    pingWorker gen sendActions = loop gen
        where
        loop g = do
            let (i, gen') = randomR (1000,2000000) g
                us = fromMicroseconds i :: Microsecond
            delay us
            _ <- knownPeers discovery
            _ <- discoverPeers discovery
            peerSet <- knownPeers discovery
            liftIO . putStrLn $ show anId ++ " has peer set: " ++ show peerSet
            forM_ (S.toList peerSet) $ \addr -> withConnectionTo sendActions (NodeId addr) $
                \(cactions :: ConversationActions Void Pong Production) -> do
                    received <- recv cactions
                    case received of
                        Just Pong -> liftIO . putStrLn $ show anId ++ " heard PONG from " ++ show addr
                        Nothing -> error "Unexpected end of input"
            loop gen'

listeners :: NodeId -> [Listener Packing Production]
listeners anId = [pongListener]
    where
    pongListener :: ListenerAction Packing Production
    pongListener = ListenerActionConversation $ \peerId (cactions :: ConversationActions Pong Void Production) -> do
        liftIO . putStrLn $ show anId ++  " heard PING from " ++ show peerId
        send cactions Pong

makeNode :: Transport Production
         -> Int
         -> Production (ThreadId Production)
makeNode transport i = do
    let port = 3000 + i
    let host = "127.0.0.1"
    let anId = makeId i
    let initialPeer =
            if i == 0
            -- First node uses itself as initial peer, else it'll panic because
            -- its initial peer appears to be down.
            then K.Node (K.Peer host (fromIntegral port)) anId
            else K.Node (K.Peer host (fromIntegral (port - 1))) (makeId (i - 1))
    let kademliaConfig = K.KademliaConfiguration (fromIntegral port) anId
    let prng1 = mkStdGen (2 * i)
    let prng2 = mkStdGen ((2 * i) + 1)
    liftIO . putStrLn $ "Starting node " ++ show i
    fork $ node transport prng1 BinaryP $ \node' -> do
        pure $ NodeAction (listeners . nodeId $ node') $ \sactions -> do
            liftIO . putStrLn $ "Making discovery for node " ++ show i
            discovery <- K.kademliaDiscovery kademliaConfig initialPeer (nodeEndPointAddress node')
            worker (nodeId node') prng2 discovery sactions `finally` closeDiscovery discovery
    where
    makeId anId
        | anId < 10 = B8.pack ("node_identifier_0" ++ show anId)
        | otherwise = B8.pack ("node_identifier_" ++ show anId)

main :: IO ()
main = runProduction $ do

    [arg0] <- liftIO getArgs
    let number = read arg0

    when (number > 99 || number < 1) $ error "Give a number in [1,99]"

    Right transport_ <- liftIO $ TCP.createTransport "0.0.0.0" "127.0.0.1" "10128" TCP.defaultTCPParameters
    let transport = concrete transport_

    liftIO . putStrLn $ "Spawning " ++ show number ++ " nodes"
    nodeThreads <- forM [0..number] (makeNode transport)

    liftIO $ putStrLn "Hit return to stop"
    _ <- liftIO $ getChar

    liftIO $ putStrLn "Stopping nodes"
    forM_ nodeThreads killThread
