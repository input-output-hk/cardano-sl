{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import GHC.Generics (Generic)
import Control.Monad (forM_)
import Data.String (fromString)
import Data.Proxy (Proxy(..))
import Data.Void (Void)
import Data.Binary
import Node
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.InMemory as InMemory
import Network.Transport.Concrete (concrete)
import System.Random
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.MVar as Conc
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as Conc
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
    liftMockable (UnGetChannel channel t) = STM.atomically $ Conc.unGetTChan channel t
    liftMockable (WriteChannel channel t) = STM.atomically $ Conc.writeTChan channel t

instance Mockable Bracket IO where
    liftMockable (Bracket acquire release act) = Exception.bracket acquire release act

instance Mockable Throw IO where
    liftMockable (Throw e) = Exception.throwIO e

instance Mockable Catch IO where
    liftMockable (Catch action handler) = action `Exception.catch` handler

-- Sending a message which encodes to "" is problematic!
-- The receiver can't distinuish this from the case in which the sender sent
-- nothing at all.
-- So we give custom Ping and Pong types with non-generic Binary instances.
--
-- TBD should we fix this in network-transport? Maybe every chunk is prefixed
-- by a byte giving its length? Wasteful I guess but maybe not a problem.

data Ping = Ping
deriving instance Show Ping
instance Binary Ping where
    put _ = putWord8 (fromIntegral 0)
    get = do
        w <- getWord8
        if w == fromIntegral 0
        then pure Ping
        else fail "no parse ping"

data Pong = Pong
deriving instance Show Pong
instance Binary Pong where
    put _ = putWord8 (fromIntegral 1)
    get = do
        w <- getWord8
        if w == fromIntegral 1
        then pure Pong
        else fail "no parse pong"

workers :: NodeId -> StdGen -> [NodeId] -> [Worker IO]
workers id gen peerIds = [pingWorker gen]
    where
    pingWorker :: StdGen -> SendActions IO -> IO ()
    pingWorker gen sendActions = loop gen
        where
        loop :: StdGen -> IO ()
        loop gen = do
            let (i, gen') = randomR (0,1000000) gen
            --putStrLn (show id ++ " is waiting for " ++ show i ++ "us before sending pings")
            Conc.threadDelay i
            let pong :: NodeId -> ConversationActions Void Pong IO -> IO ()
                pong peerId cactions = do
                    putStrLn (show id ++ " sent PING to " ++ show peerId)
                    Pong <- recv cactions
                    putStrLn (show id ++ " heard PONG from " ++ show peerId)
            forM_ peerIds $ \peerId -> withConnectionTo sendActions peerId (fromString "ping") (pong peerId)
            loop gen'

listeners :: NodeId -> [Listener IO]
listeners id = [Listener (fromString "ping") pongWorker]
    where
    pongWorker :: ListenerAction IO
    pongWorker = ListenerActionConversation $ \peerId (cactions :: ConversationActions Pong Void IO) -> do
        putStrLn (show id ++  " heard PING from " ++ show peerId)
        send cactions Pong
        putStrLn (show id ++ " sent PONG to " ++ show peerId)

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
