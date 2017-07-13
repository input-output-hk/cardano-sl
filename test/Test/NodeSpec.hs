{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}

module Test.NodeSpec
       ( spec
       ) where

import           Control.Monad               (forM_, when)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Lens                (sans, (%=), (&~), (.=))
import           Control.Exception           (AsyncException(..))
import           Data.Foldable               (for_)
import qualified Data.Set                    as S
import           Data.Time.Units             (Microsecond)
import           Test.Hspec                  (Spec, describe, runIO, afterAll_)
import           Test.Hspec.QuickCheck       (prop, modifyMaxSuccess)
import           Test.QuickCheck             (Property, ioProperty)
import           Test.QuickCheck.Modifiers   (NonEmptyList(..), getNonEmpty)
import           Test.Util                   (HeavyParcel (..), Parcel (..),
                                              TalkStyle (..), TestState, deliveryTest,
                                              expected, mkTestState, modifyTestState,
                                              newWork, receiveAll, sendAll,
                                              makeTCPTransport, makeInMemoryTransport,
                                              Payload(..), timeout)
import           System.Random               (newStdGen)
import qualified Network.Transport           as NT (Transport)
import qualified Network.Transport.Abstract  as NT
                                             (closeTransport, newEndPoint,
                                              closeEndPoint, address, receive)
import           Network.Transport.TCP       (simpleOnePlaceQDisc, simpleUnboundedQDisc)
import           Network.QDisc.Fair          (fairQDisc)
import           Network.Transport.Concrete  (concrete)
import           Mockable.Class              (Mockable)
import           Mockable.SharedExclusive    (newSharedExclusive, readSharedExclusive,
                                              putSharedExclusive, takeSharedExclusive,
                                              tryPutSharedExclusive, SharedExclusive)
import           Mockable.Concurrent         (withAsync, wait, Async, Delay, delay)
import           Mockable.Exception          (catch, throw)
import           Mockable.Production         (Production, runProduction)
import           Node.Message.Binary         (BinaryP(..))
import           Node
import           Node.Conversation
import           Node.OutboundQueue

spec :: Spec
spec = describe "Node" $ modifyMaxSuccess (const 50) $ do

    -- Take at most 25000 bytes for each Received message.
    -- We want to ensure that the MTU works, but not make the tests too
    -- painfully slow.
    let mtu = 25000
    let tcpTransportUnbounded = runIO $ makeTCPTransport "0.0.0.0" "127.0.0.1" "10342" simpleUnboundedQDisc mtu
    let tcpTransportOnePlace = runIO $ makeTCPTransport "0.0.0.0" "127.0.0.1" "10343" simpleOnePlaceQDisc mtu
    let tcpTransportFair = runIO $ makeTCPTransport "0.0.0.0" "127.0.0.1" "10345" (fairQDisc (const (return Nothing))) mtu
    let memoryTransport = runIO $ makeInMemoryTransport
    let transports = [
              ("TCP unbounded queueing", tcpTransportUnbounded)
            , ("TCP one-place queueing", tcpTransportOnePlace)
            , ("TCP fair queueing", tcpTransportFair)
            , ("In-memory", memoryTransport)
            ]
    let nodeEnv = defaultNodeEnvironment { nodeMtu = mtu }

    let mkOutboundQueue
            :: forall peerData .
               Converse BinaryP peerData Production
            -> Production (OutboundQueue BinaryP peerData NodeId () Production)
        mkOutboundQueue converse = pure (freeForAll id converse)

    forM_ transports $ \(name, mkTransport) -> do

        transport_ <- mkTransport
        let transport = concrete transport_

        describe ("Using transport: " ++ name) $ afterAll_ (runProduction (NT.closeTransport transport)) $ do

            prop "peer data" $ ioProperty . runProduction $ do
                clientGen <- liftIO newStdGen
                serverGen <- liftIO newStdGen
                serverAddressVar <- newSharedExclusive
                clientFinished <- newSharedExclusive
                serverFinished <- newSharedExclusive
                let attempts = 1

                let listener = Listener $ \pd _ _ cactions -> do
                        True <- return $ pd == ("client", 24)
                        initial <- timeout "server waiting for request" 30000000 (recv cactions maxBound)
                        case initial of
                            Nothing -> error "got no initial message"
                            Just (Parcel i (Payload _)) -> do
                                _ <- timeout "server sending response" 30000000 (send cactions (Parcel i (Payload 32)))
                                return ()

                let server = node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) mkOutboundQueue serverGen BinaryP ("server" :: String, 42 :: Int) nodeEnv $ \_node ->
                        NodeAction (const [listener]) $ \sendActions -> do
                            putSharedExclusive serverAddressVar (nodeId _node)
                            takeSharedExclusive clientFinished
                            putSharedExclusive serverFinished ()

                let client = node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) mkOutboundQueue clientGen BinaryP ("client" :: String, 24 :: Int) nodeEnv $ \_node ->
                        NodeAction (const [listener]) $ \sendActions -> do
                            serverAddress <- readSharedExclusive serverAddressVar
                            forM_ [1..attempts] $ \i -> withConnectionTo sendActions serverAddress $ \peerData -> Conversation $ \cactions -> do
                                True <- return $ peerData == ("server", 42)
                                _ <- timeout "client sending" 30000000 (send cactions (Parcel i (Payload 32)))
                                response <- timeout "client waiting for response" 30000000 (recv cactions maxBound)
                                case response of
                                    Nothing -> error "got no response"
                                    Just (Parcel j (Payload _)) -> do
                                        when (j /= i) (error "parcel number mismatch")
                                        return ()
                            putSharedExclusive clientFinished ()
                            takeSharedExclusive serverFinished

                withAsync server $ \serverPromise -> do
                    withAsync client $ \clientPromise -> do
                        wait clientPromise
                        wait serverPromise

                return True

            -- Test where a node converses with itself. Fails only if an exception is
            -- thrown.
            prop "self connection" $ ioProperty . runProduction $ do
                gen <- liftIO newStdGen
                -- Self-connections don't make TCP sockets so we can do an absurd amount
                -- of attempts without taking too much time.
                let attempts = 100

                let listener = Listener $ \pd _ _ cactions -> do
                        True <- return $ pd == ("some string", 42)
                        initial <- recv cactions maxBound
                        case initial of
                            Nothing -> error "got no initial message"
                            Just (Parcel i (Payload _)) -> do
                                _ <- send cactions (Parcel i (Payload 32))
                                return ()

                node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) mkOutboundQueue gen BinaryP ("some string" :: String, 42 :: Int) nodeEnv $ \_node ->
                    NodeAction (const [listener]) $ \sendActions -> do
                        forM_ [1..attempts] $ \i -> withConnectionTo sendActions (nodeId _node) $ \peerData -> Conversation $ \cactions -> do
                            True <- return $ peerData == ("some string", 42)
                            _ <- send cactions (Parcel i (Payload 32))
                            response <- recv cactions maxBound
                            case response of
                                Nothing -> error "got no response"
                                Just (Parcel j (Payload _)) -> do
                                    when (j /= i) (error "parcel number mismatch")
                                    return ()
                return True

            prop "ack timeout" $ ioProperty . runProduction $ do
                gen <- liftIO newStdGen
                let env = nodeEnv {
                          -- 1/10 second.
                          nodeAckTimeout = 100000
                        }
                -- An endpoint to which the node will connect. It will never
                -- respond to the node's SYN.
                Right ep <- NT.newEndPoint transport
                let peerAddr = NodeId (NT.address ep)
                -- Must clear the endpoint's receive queue so that it's
                -- never blocked on enqueue.
                withAsync (let loop = NT.receive ep >> loop in loop) $ \clearQueue -> do
                    -- We want withConnectionTo to get a Timeout exception, as
                    -- delivered by withConnectionTo in case of an ACK timeout.
                    -- A ThreadKilled would come from the outer 'timeout', the
                    -- testing utility.
                    let handleThreadKilled :: Timeout -> Production ()
                        handleThreadKilled Timeout = do
                            --liftIO . putStrLn $ "Thread killed successfully!"
                            return ()
                    node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) mkOutboundQueue gen BinaryP () env $ \_node ->
                        NodeAction (const []) $ \sendActions -> do
                            timeout "client waiting for ACK" 5000000 $
                                flip catch handleThreadKilled $ withConnectionTo sendActions peerAddr $ \peerData -> Conversation $ \cactions -> do
                                    _ :: Maybe Parcel <- recv cactions maxBound
                                    send cactions (Parcel 0 (Payload 32))
                                    return ()
                    --liftIO . putStrLn $ "Closing end point"
                    NT.closeEndPoint ep
                --liftIO . putStrLn $ "Closed end point"
                return True

            -- one sender, one receiver
            describe "delivery" $ do
                for_ [ConversationStyle] $ \talkStyle -> do
                    prop "plain" $
                        plainDeliveryTest transport_ nodeEnv talkStyle
                    prop "heavy messages sent nicely" $
                        withHeavyParcels $ plainDeliveryTest transport_ nodeEnv talkStyle

prepareDeliveryTestState :: [Parcel] -> IO (TVar TestState)
prepareDeliveryTestState expectedParcels =
    newTVarIO $ mkTestState &~
        expected .= S.fromList expectedParcels

plainDeliveryTest
    :: NT.Transport
    -> NodeEnvironment Production
    -> TalkStyle
    -> NonEmptyList Parcel
    -> Property
plainDeliveryTest transport_ nodeEnv talkStyle neparcels = ioProperty $ do
    let parcels = getNonEmpty neparcels
    testState <- prepareDeliveryTestState parcels

    let worker peerId sendActions = newWork testState "client" $
            sendAll talkStyle sendActions peerId parcels

        listener = receiveAll talkStyle $
            \parcel -> modifyTestState testState $ expected %= sans parcel

    deliveryTest transport_ nodeEnv testState [worker] [listener]

withHeavyParcels :: (NonEmptyList Parcel -> Property) -> NonEmptyList HeavyParcel -> Property
withHeavyParcels testCase (NonEmpty megaParcels) = testCase (NonEmpty (getHeavyParcel <$> megaParcels))
