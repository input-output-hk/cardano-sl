{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}

module Test.NodeSpec
       ( spec
       ) where


import           Control.Concurrent.Async (wait, withAsync)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar,
                     takeMVar)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Exception (handle, throwIO)
import           Control.Lens (sans, (%=), (&~), (.=))
import           Control.Monad (forM_, unless, when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Set as S
import           Network.QDisc.Fair (fairQDisc)
import qualified Network.Transport as NT (Transport, address, closeEndPoint,
                     closeTransport, newEndPoint, receive)
import           Network.Transport.TCP (simpleOnePlaceQDisc,
                     simpleUnboundedQDisc)
import           System.Random (newStdGen)
import           Test.Hspec (Spec, afterAll_, describe, runIO)
import           Test.Hspec.Core.Spec (SpecM)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, ioProperty)
import           Test.QuickCheck.Modifiers (NonEmptyList (..), getNonEmpty)

import           Node
import           Node.Message.Binary (binaryPacking)
import           Pos.Util.Log.LoggerConfig (defaultTestConfiguration)
import           Pos.Util.Trace (wsetupLogging)
import           Pos.Util.Wlog (Severity (Debug))
import           Test.Util (HeavyParcel (..), Parcel (..), Payload (..),
                     TestState, deliveryTest, expected, makeInMemoryTransport,
                     makeTCPTransport, mkTestState, modifyTestState,
                     receiveAll, sendAll, timeout)

spec :: Spec
spec = describe "Node" $ modifyMaxSuccess (const 50) $ do

    logTrace <- runIO $ wsetupLogging (defaultTestConfiguration Debug) ""

        -- Take at most 25000 bytes for each Received message.
        -- We want to ensure that the MTU works, but not make the tests too
        -- painfully slow.
    let mtu = 25000
        tcpTransportUnbounded = runIO $ makeTCPTransport "0.0.0.0" "127.0.0.1" "10342" simpleUnboundedQDisc mtu
        tcpTransportOnePlace = runIO $ makeTCPTransport "0.0.0.0" "127.0.0.1" "10343" simpleOnePlaceQDisc mtu
        tcpTransportFair = runIO $ makeTCPTransport "0.0.0.0" "127.0.0.1" "10345" (fairQDisc (const (return Nothing))) mtu
        memoryTransport = runIO $ makeInMemoryTransport

        -- need this to avoid ambiguous type warnings/errors
        s :: String -> String
        s = id

        transports :: [(String, SpecM () NT.Transport)]
        transports = [
            -- Disable the tests over TCP transport for now, because the CI
            -- machines apparently cannot run them due to OS network
            -- configuration problems. They're seq'd so that we don't have to
            -- remove all the relevant imports (-Wall -Werror).
              (s "TCP unbounded queueing", tcpTransportUnbounded) `seq`
              (s "TCP one-place queueing", tcpTransportOnePlace) `seq`
              (s "TCP fair queueing", tcpTransportFair) `seq`
              ("In-memory", memoryTransport)
            ]
        nodeEnv = defaultNodeEnvironment { nodeMtu = mtu }

    forM_ transports $ \(name, mkTransport) -> do

        transport <- mkTransport

        describe ("Using transport: " ++ name) $ afterAll_ (NT.closeTransport transport) $ do

            prop "peer data" $ ioProperty $ do
                clientGen <- liftIO newStdGen
                serverGen <- liftIO newStdGen
                serverAddressVar <- newEmptyMVar
                clientFinished <- newEmptyMVar
                serverFinished <- newEmptyMVar
                let attempts = 1

                let listener = Listener $ \pd _ cactions -> do
                        unless (pd == ("client", 24)) (error "bad pd")
                        initial <- timeout "server waiting for request" 30000000 (recv cactions maxBound)
                        case initial of
                            Nothing -> error "got no initial message"
                            Just (Parcel i (Payload _)) -> do
                                _ <- timeout "server sending response" 30000000 (send cactions (Parcel i (Payload 32)))
                                return ()

                let server = node logTrace (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) serverGen binaryPacking ("server" :: String, 42 :: Int) nodeEnv $ \_node ->
                        NodeAction (const [listener]) $ \_converse -> do
                            putMVar serverAddressVar (nodeId _node)
                            takeMVar clientFinished
                            putMVar serverFinished ()

                let client = node logTrace (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) clientGen binaryPacking ("client" :: String, 24 :: Int) nodeEnv $ \_node ->
                        NodeAction (const [listener]) $ \converse -> do
                            serverAddress <- readMVar serverAddressVar
                            forM_ [1..attempts] $ \i -> converseWith converse serverAddress $ \peerData -> Conversation $ \cactions -> do
                                unless (peerData == ("server", 42)) (error "bad peer data")
                                _ <- timeout "client sending" 30000000 (send cactions (Parcel i (Payload 32)))
                                response <- timeout "client waiting for response" 30000000 (recv cactions maxBound)
                                case response of
                                    Nothing -> error "got no response"
                                    Just (Parcel j (Payload _)) -> do
                                        when (j /= i) (error "parcel number mismatch")
                                        return ()
                            putMVar clientFinished ()
                            takeMVar serverFinished

                withAsync server $ \serverPromise -> do
                    withAsync client $ \clientPromise -> do
                        wait clientPromise
                        wait serverPromise

                return True

            -- Test where a node converses with itself. Fails only if an exception is
            -- thrown.
            prop "self connection" $ ioProperty $ do
                gen <- liftIO newStdGen
                -- Self-connections don't make TCP sockets so we can do an absurd amount
                -- of attempts without taking too much time.
                let attempts = 100

                let listener = Listener $ \pd _ cactions -> do
                        unless (pd == ("some string", 42)) (error "bad pd")
                        initial <- recv cactions maxBound
                        case initial of
                            Nothing -> error "got no initial message"
                            Just (Parcel i (Payload _)) -> do
                                _ <- send cactions (Parcel i (Payload 32))
                                return ()

                node logTrace (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) gen binaryPacking ("some string" :: String, 42 :: Int) nodeEnv $ \_node ->
                    NodeAction (const [listener]) $ \converse -> do
                        forM_ [1..attempts] $ \i -> converseWith converse (nodeId _node) $ \peerData -> Conversation $ \cactions -> do
                            unless (peerData == ("some string", 42)) (error "bad peer data")
                            _ <- send cactions (Parcel i (Payload 32))
                            response <- recv cactions maxBound
                            case response of
                                Nothing -> error "got no response"
                                Just (Parcel j (Payload _)) -> do
                                    when (j /= i) (error "parcel number mismatch")
                                    return ()
                return True

            prop "ack timeout" $ ioProperty $ do
                gen <- liftIO newStdGen
                let env = nodeEnv {
                          -- 1/10 second.
                          nodeAckTimeout = 100000
                        }
                -- An endpoint to which the node will connect. It will never
                -- respond to the node's SYN.
                ep <- NT.newEndPoint transport >>= either throwIO return
                let peerAddr = NodeId (NT.address ep)
                -- Must clear the endpoint's receive queue so that it's
                -- never blocked on enqueue.
                withAsync (let loop = NT.receive ep >> loop in loop) $ \_clearQueue -> do
                    -- We want withConnectionTo to get a Timeout exception, as
                    -- delivered by withConnectionTo in case of an ACK timeout.
                    -- A ThreadKilled would come from the outer 'timeout', the
                    -- testing utility.
                    let handleThreadKilled :: Timeout -> IO ()
                        handleThreadKilled Timeout = do
                            --liftIO . putStrLn $ "Thread killed successfully!"
                            return ()
                    node logTrace (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) gen binaryPacking () env $ \_node ->
                        NodeAction (const []) $ \converse -> do
                            timeout "client waiting for ACK" 5000000 $
                                handle handleThreadKilled $ converseWith converse peerAddr $ \_peerData -> Conversation $ \cactions -> do
                                    _ :: Maybe Parcel <- recv cactions maxBound
                                    send cactions (Parcel 0 (Payload 32))
                                    return ()
                    --liftIO . putStrLn $ "Closing end point"
                    NT.closeEndPoint ep
                --liftIO . putStrLn $ "Closed end point"
                return True

            -- one sender, one receiver
            describe "delivery" $ do
                prop "plain" $
                    plainDeliveryTest transport nodeEnv
                prop "heavy messages sent nicely" $
                    withHeavyParcels $ plainDeliveryTest transport nodeEnv

prepareDeliveryTestState :: [Parcel] -> IO (TVar TestState)
prepareDeliveryTestState expectedParcels =
    newTVarIO $ mkTestState &~
        expected .= S.fromList expectedParcels

plainDeliveryTest
    :: NT.Transport
    -> NodeEnvironment
    -> NonEmptyList Parcel
    -> Property
plainDeliveryTest transport nodeEnv neparcels = ioProperty $ do
    let parcels = getNonEmpty neparcels
    testState <- prepareDeliveryTestState parcels

    let worker peerId converse = sendAll converse peerId parcels

        listener = receiveAll $
            \parcel -> modifyTestState testState $ expected %= sans parcel

    deliveryTest transport nodeEnv testState [worker] [listener]

withHeavyParcels :: (NonEmptyList Parcel -> Property) -> NonEmptyList HeavyParcel -> Property
withHeavyParcels testCase (NonEmpty megaParcels) = testCase (NonEmpty (getHeavyParcel <$> megaParcels))
