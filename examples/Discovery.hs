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
import           Data.String                          (fromString)
import           Data.Void                            (Void)
import           Mockable.Concurrent                  (delay)
import           Mockable.Production
import           Network.Discovery.Abstract
import qualified Network.Discovery.Transport.Kademlia as K
import           Network.Transport.Abstract           (newEndPoint)
import           Network.Transport.Concrete           (concrete)
import qualified Network.Transport.InMemory           as InMemory
import qualified Network.Transport.TCP                as TCP
import           Node
import           System.Environment                   (getArgs)
import           System.Random

data Pong = Pong
deriving instance Show Pong
instance Binary Pong where
    put _ = putWord8 (fromIntegral 1)
    get = do
        w <- getWord8
        if w == fromIntegral 1
        then pure Pong
        else fail "no parse pong"

type Header = ()

workers :: NodeId -> StdGen -> NetworkDiscovery K.KademliaDiscoveryErrorCode Production -> [Worker Header Production]
workers id gen discovery = [pingWorker gen]
    where
    pingWorker :: StdGen -> SendActions Header Production -> Production ()
    pingWorker gen sendActions = loop gen
        where
        loop gen = do
            let (i, gen') = randomR (1000,2000000) gen
            delay i
            peerSet_ <- knownPeers discovery
            discoverPeers discovery
            peerSet <- knownPeers discovery
            liftIO . putStrLn $ show id ++ " has peer set: " ++ show peerSet
            forM_ (S.toList peerSet) $ \addr -> withConnectionTo sendActions (NodeId addr) (fromString "ping") $
                \(cactions :: ConversationActions Header Void Pong Production) -> do
                    received <- recv cactions
                    case received of
                        Just Pong -> liftIO . putStrLn $ show id ++ " heard PONG from " ++ show addr
                        Nothing -> error "Unexpected end of input"
            loop gen'

listeners :: NodeId -> [Listener Header Production]
listeners id = [Listener (fromString "ping") pongListener]
    where
    pongListener :: ListenerAction Header Production
    pongListener = ListenerActionConversation $ \peerId (cactions :: ConversationActions Header Pong Void Production) -> do
        liftIO . putStrLn $ show id ++  " heard PING from " ++ show peerId
        send cactions () Pong

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
    Right endPoint <- newEndPoint transport
    rec { node <- startNode @() endPoint prng1 (workers (nodeId node) prng2 discovery)
            Nothing (listeners (nodeId node))
        ; let localAddress = nodeEndPointAddress node
        ; liftIO . putStrLn $ "Making discovery for node " ++ show i
        ; discovery <- K.kademliaDiscovery kademliaConfig initialPeer localAddress
        }
    pure (node, discovery)
    where
    makeId i
        | i < 10 = B8.pack ("node_identifier_0" ++ show i)
        | otherwise = B8.pack ("node_identifier_" ++ show i)

main = runProduction $ do

    [arg0] <- liftIO getArgs
    let number = read arg0

    when (number > 99 || number < 1) $ error "Give a number in [1,99]"

    --transport_ <- liftIO $ InMemory.createTransport
    Right transport_ <- liftIO $ TCP.createTransport ("127.0.0.1") ("10128") TCP.defaultTCPParameters
    let transport = concrete transport_

    liftIO . putStrLn $ "Spawning " ++ show number ++ " nodes"
    nodesAndDiscoveries <- forM [0..number] (makeNode transport)

    liftIO $ putStrLn "Hit return to stop"
    _ <- liftIO $ getChar

    liftIO $ putStrLn "Stopping nodes"
    forM_ nodesAndDiscoveries (\(n, d) -> stopNode n >> closeDiscovery d)
