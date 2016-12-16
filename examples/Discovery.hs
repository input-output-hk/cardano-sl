{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Monad (forM_, forM)
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
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.STM.TChan as Conc
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.MVar as Conc
import qualified Control.Exception as Exception
import Mockable.Class
import Mockable.Concurrent
import Mockable.SharedAtomic
import Mockable.Channel
import Mockable.Exception

type instance ThreadId IO = Conc.ThreadId

instance Mockable Fork IO where
    liftMockable (Fork m) = Conc.forkIO m
    liftMockable (MyThreadId) = Conc.myThreadId
    liftMockable (KillThread tid) = Conc.killThread tid

instance Mockable RunInUnboundThread IO where
    liftMockable (RunInUnboundThread m) = Conc.runInUnboundThread m

type instance SharedAtomicT IO = Conc.MVar

instance Mockable SharedAtomic IO where
    liftMockable (NewSharedAtomic t) = Conc.newMVar t
    liftMockable (ModifySharedAtomic atomic f) = Conc.modifyMVar atomic f

type instance ChannelT IO = Conc.TChan

instance Mockable Channel IO where
    liftMockable (NewChannel) = STM.atomically Conc.newTChan
    liftMockable (ReadChannel channel) = STM.atomically $ Conc.readTChan channel
    liftMockable (TryReadChannel channel) = STM.atomically $ Conc.tryReadTChan channel
    liftMockable (WriteChannel channel t) = STM.atomically $ Conc.writeTChan channel t

instance Mockable Bracket IO where
    liftMockable (Bracket acquire release act) = Exception.bracket acquire release act

instance Mockable Throw IO where
    liftMockable (Throw e) = Exception.throwIO e

workers :: ( RandomGen g ) => NodeId -> g -> NetworkDiscovery K.KademliaDiscoveryErrorCode IO -> [Worker IO]
workers id gen discovery = [pingWorker gen]
    where
    pingWorker :: ( RandomGen g ) => g -> SendActions IO -> IO ()
    pingWorker gen sendActions = loop gen
        where
        loop gen = do
            let (i, gen') = randomR (1000,2000000) gen
            --putStrLn (show id ++ " is waiting for " ++ show i ++ "us before discovering peers and sending pings")
            Conc.threadDelay i
            _ <- discoverPeers discovery
            peerSet <- knownPeers discovery
            putStrLn (show id ++ " has peer set: " ++ show peerSet)
            forM_ (S.toList peerSet) (\addr -> sendTo sendActions (NodeId addr) (fromString "ping") ())
            loop gen'

listeners :: NodeId -> [Listener IO]
listeners id = [Listener (fromString "ping") pongWorker]
    where
    pongWorker :: ListenerAction IO
    pongWorker = ListenerActionOneMsg $ \peerId sendActions () -> do
        putStrLn (show id ++  " heard a ping from " ++ show peerId)

makeNode transport i = mdo
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
    putStrLn $ "Starting node " ++ show i
    node <- startNode transport prng1 (workers (nodeId node) prng2 discovery) (listeners (nodeId node))
    let localAddress = nodeEndPointAddress node
    putStrLn $ "Making discovery for node " ++ show i
    discovery <- K.kademliaDiscovery kademliaConfig initialPeer localAddress
    pure (node, discovery)
    where
    makeId i = B8.pack ("node_identifier_" ++ show i)

main = mdo

    [arg0] <- getArgs
    let number = read arg0

    Right transport_ <- TCP.createTransport ("127.0.0.1") ("10128") TCP.defaultTCPParameters
    let transport = concrete transport_

    putStrLn $ "Spawning " ++ show number ++ " nodes"
    nodesAndDiscoveries <- forM [0..number] (makeNode transport)

    putStrLn "Hit return to stop"
    _ <- getChar

    putStrLn "Stopping nodes"
    forM_ nodesAndDiscoveries (\(n, d) -> stopNode n >> closeDiscovery d)
