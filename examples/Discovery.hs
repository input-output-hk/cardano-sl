{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B8
import Node
import qualified Network.Transport.TCP as TCP
import Network.Transport.Concrete (concrete)
import Network.Discovery.Abstract
import qualified Network.Discovery.Transport.Kademlia as K
import System.Environment (getArgs)
import System.Random
import Mockable.Concurrent (delay)
import Mockable.Production

workers :: ( RandomGen g ) => NodeId -> g -> NetworkDiscovery K.KademliaDiscoveryErrorCode Production -> [Worker Production]
workers id gen discovery = [pingWorker gen]
    where
    pingWorker :: ( RandomGen g ) => g -> SendActions Production -> Production ()
    pingWorker gen sendActions = loop gen
        where
        loop gen = do
            let (i, gen') = randomR (1000,2000000) gen
            --putStrLn (show id ++ " is waiting for " ++ show i ++ "us before discovering peers and sending pings")
            delay i
            _ <- discoverPeers discovery
            peerSet <- knownPeers discovery
            liftIO . putStrLn $ show id ++ " has peer set: " ++ show peerSet
            forM_ (S.toList peerSet) (\addr -> sendTo sendActions (NodeId addr) (fromString "ping") ())
            loop gen'

listeners :: NodeId -> [Listener Production]
listeners id = [Listener (fromString "ping") pongWorker]
    where
    pongWorker :: ListenerAction Production
    pongWorker = ListenerActionOneMsg $ \peerId sendActions () -> do
        liftIO . putStrLn $ show id ++  " heard a ping from " ++ show peerId

makeNode transport i = do
    let port = 3000 + i
    let host = "127.0.0.1"
    let id = makeId i
    let initialPeer =
            if i == 0
            -- First node uses itself as initial peer, else it'll panic because
            -- its initial peer appears to be down.
            then K.Node (K.Peer host (fromIntegral port)) id
            else K.Node (K.Peer host (fromIntegral (port - 1))) (makeId (i - 1))
    let kademliaConfig = K.KademliaConfiguration (fromIntegral port) id
    let prng1 = mkStdGen (2 * i)
    let prng2 = mkStdGen ((2 * i) + 1)
    liftIO . putStrLn $ "Starting node " ++ show i
    rec { node <- startNode transport prng1 (workers (nodeId node) prng2 discovery) (listeners (nodeId node))
        ; let localAddress = nodeEndPointAddress node
        ; liftIO . putStrLn $ "Making discovery for node " ++ show i
        ; discovery <- K.kademliaDiscovery kademliaConfig initialPeer localAddress
        }
    pure (node, discovery)
    where
    makeId i = B8.pack ("node_identifier_" ++ show i)

main = runProduction $ do

    [arg0] <- liftIO getArgs
    let number = read arg0

    Right transport_ <- liftIO $ TCP.createTransport ("127.0.0.1") ("10128") TCP.defaultTCPParameters
    let transport = concrete transport_

    liftIO . putStrLn $ "Spawning " ++ show number ++ " nodes"
    nodesAndDiscoveries <- forM [0..number] (makeNode transport)

    liftIO $ putStrLn "Hit return to stop"
    _ <- liftIO $ getChar

    liftIO $ putStrLn "Stopping nodes"
    forM_ nodesAndDiscoveries (\(n, d) -> stopNode n >> closeDiscovery d)
