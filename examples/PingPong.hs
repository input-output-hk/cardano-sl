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
import           Data.Data                  (Data)
import           Data.Time.Units            (Microsecond, fromMicroseconds)
import           GHC.Generics               (Generic)
import           Message.Message            (BinaryP (..))
import           Mockable.Concurrent        (delay, for, fork, killThread)
import           Mockable.Production
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP      as TCP
import           Node
import           System.Random

-- | Type for messages from the workers to the listeners.
data Ping = Ping
deriving instance Generic Ping
deriving instance Data Ping
deriving instance Show Ping
instance Binary Ping
instance Message Ping where
    formatMessage _ = "Ping"

-- | Type for messages from the listeners to the workers.
data Pong = Pong
deriving instance Generic Pong
deriving instance Show Pong
instance Binary Pong

type Packing = BinaryP

worker :: NodeId -> StdGen -> [NodeId] -> Worker Packing Production
worker anId generator peerIds = pingWorker generator
    where
    pingWorker :: StdGen -> SendActions Packing Production -> Production ()
    pingWorker gen sendActions = loop gen
        where
        loop :: StdGen -> Production ()
        loop g = do
            let (i, gen') = randomR (0,1000000) g
                us = fromMicroseconds i :: Microsecond
            delay (for us)
            let pong :: NodeId -> ConversationActions Ping Pong Production -> Production ()
                pong peerId cactions = do
                    liftIO . putStrLn $ show anId ++ " sent PING to " ++ show peerId
                    received <- recv cactions
                    case received of
                        Just Pong -> liftIO . putStrLn $ show anId ++ " heard PONG from " ++ show peerId
                        Nothing -> error "Unexpected end of input"
            forM_ peerIds $ \peerId -> withConnectionTo sendActions peerId (pong peerId)
            loop gen'

listeners :: NodeId -> [Listener Packing Production]
listeners anId = [pongWorker]
    where
    pongWorker :: ListenerAction Packing Production
    pongWorker = ListenerActionConversation $ \peerId (cactions :: ConversationActions Pong Ping Production) -> do
        liftIO . putStrLn $ show anId ++  " heard PING from " ++ show peerId
        send cactions Pong
        liftIO . putStrLn $ show anId ++ " sent PONG to " ++ show peerId

main :: IO ()
main = runProduction $ do

    Right transport_ <- liftIO $ TCP.createTransport ("127.0.0.1") ("10128") TCP.defaultTCPParameters
    let transport = concrete transport_

    let prng1 = mkStdGen 0
    let prng2 = mkStdGen 1
    let prng3 = mkStdGen 2
    let prng4 = mkStdGen 3

    liftIO . putStrLn $ "Starting nodes"
    node transport prng1 BinaryP $ \node1 ->
        pure $ NodeAction (listeners . nodeId $ node1) $ \sactions1 ->
        node transport prng2 BinaryP $ \node2 ->
        pure $ NodeAction (listeners . nodeId $ node2) $ \sactions2 -> do
        tid1 <- fork $ worker (nodeId node1) prng3 [nodeId node2] sactions1
        tid2 <- fork $ worker (nodeId node2) prng4 [nodeId node1] sactions2
        liftIO . putStrLn $ "Hit return to stop"
        _ <- liftIO getChar
        killThread tid1
        killThread tid2
        liftIO . putStrLn $ "Stopping nodes"
    liftIO . putStrLn $ "All done."
