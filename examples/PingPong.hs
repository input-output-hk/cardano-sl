{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Monad (forM_)
import Data.String (fromString)
import Node
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.InMemory as InMemory
import Network.Transport.Concrete (concrete)
import System.Random
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Chan as Conc
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

type instance ChannelT IO = Conc.Chan

instance Mockable Channel IO where
    liftMockable (NewChannel) = Conc.newChan
    liftMockable (ReadChannel channel) = Conc.readChan channel
    liftMockable (WriteChannel channel t) = Conc.writeChan channel t

instance Mockable Bracket IO where
    liftMockable (Bracket acquire release act) = Exception.bracket acquire release act

instance Mockable Throw IO where
    liftMockable (Throw e) = Exception.throwIO e

workers :: ( RandomGen g ) => NodeId -> g -> [NodeId] -> [Worker IO]
workers id gen peerIds = [pingWorker gen]
    where
    pingWorker :: ( RandomGen g ) => g -> SendActions IO -> IO ()
    pingWorker gen sendActions = loop gen
        where
        loop gen = do
            let (i, gen') = randomR (0,1000000) gen
            putStrLn (show id ++ " is waiting for " ++ show i ++ "us before sending pings")
            Conc.threadDelay i
            forM_ peerIds (\peerId -> sendTo sendActions peerId (fromString "ping") ())
            loop gen'

listeners :: NodeId -> [Listener IO]
listeners id = [Listener (fromString "ping") pongWorker]
    where
    pongWorker :: ListenerAction IO
    pongWorker = ListenerActionOneMsg $ \peerId sendActions () -> do
        putStrLn (show id ++  " heard a pong from " ++ show peerId)

main = mdo

    --transport_ <- InMemory.createTransport
    Right transport_ <- TCP.createTransport ("127.0.0.1") ("10128") TCP.defaultTCPParameters
    let transport = concrete transport_

    let prng1 = mkStdGen 0
    let prng2 = mkStdGen 1
    let prng3 = mkStdGen 2
    let prng4 = mkStdGen 3

    putStrLn "Starting nodes"
    rec { node1 <- startNode transport prng1 (workers nodeId1 prng2 [nodeId2]) (listeners nodeId1)
        ; node2 <- startNode transport prng3 (workers nodeId2 prng4 [nodeId1]) (listeners nodeId2)
        ; let nodeId1 = nodeId node1
        ; let nodeId2 = nodeId node2
        }

    putStrLn "Hit return to stop"
    _ <- getChar

    putStrLn "Stopping node"
    stopNode node1
    stopNode node2
