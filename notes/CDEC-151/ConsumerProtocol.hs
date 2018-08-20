{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies,
             FlexibleContexts, ScopedTypeVariables, GADTs, RankNTypes,
             GeneralizedNewtypeDeriving #-}
module ConsumerProtocol where

import Data.Word
import Data.List (tails, foldl')
--import Data.Maybe
import Data.Hashable
--import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM (STM, retry)
import Control.Exception (assert)
import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import System.Random (StdGen, mkStdGen, randomR)

import Test.QuickCheck

import ChainExperiment2
import MonadClass
import Sim hiding (MVar)
import qualified Sim

--
-- IPC based protocol
--



-- | In this protocol the consumer always initiates things and the producer
-- replies. This is the type of messages that the consumer sends.
data MsgConsumer = MsgRequestNext
                 | MsgSetHead Point [Point]

-- | This is the type of messages that the producer sends.
data MsgProducer = MsgRollForward  Block
                 | MsgRollBackward Point
                 | MsgAwaitReply
                 | MsgIntersectImproved Point Point
                 | MsgIntersectUnchanged

data ConsumerHandlers m = ConsumerHandlers {
       getChainPoints :: m (Point, [Point]),
       addBlock       :: Block -> m (),
       rollbackTo     :: Point -> m ()
     }

consumerSideProtocol1 :: forall m.
                         (MonadSendRecv m, MonadSay m)
                      => ConsumerHandlers m
                      -> BiChan m MsgConsumer MsgProducer
                      -> m ()
consumerSideProtocol1 ConsumerHandlers{..} chan = do
    -- The consumer opens by sending a list of points on their chain.
    -- This includes the head block and 
    (hpoint, points) <- getChainPoints
    sendMsg chan (MsgSetHead hpoint points)
    MsgIntersectImproved{} <- recvMsg chan
    requestNext
  where
    requestNext :: m ()
    requestNext = do
      sendMsg chan MsgRequestNext
      reply <- recvMsg chan
      handleChainUpdate reply
      requestNext

    handleChainUpdate :: MsgProducer -> m ()
    handleChainUpdate MsgAwaitReply = do
      say ("awaiting real reply")

    handleChainUpdate (MsgRollForward  b) = do
      addBlock b
      say ("ap blocks from point X to point Y")
      return ()

    handleChainUpdate (MsgRollBackward p) = do
      say ("rolling back N blocks from point X to point Y")
      return ()


data ProducerHandlers m r = ProducerHandlers {
       findIntersectionRange :: Point -> [Point] -> m (Maybe (Point, Point)),
       establishReaderState  :: Point -> Point -> m r,
       updateReaderState     :: r -> Point -> Maybe Point -> m (),
       tryReadChainUpdate    :: r -> m (Maybe (ConsumeChain Block)),
       readChainUpdate       :: r -> m (ConsumeChain Block)
     }

producerSideProtocol1 :: forall m r.
                         (MonadSendRecv m, MonadSay m)
                      => ProducerHandlers m r
                      -> BiChan m MsgProducer MsgConsumer
                      -> m ()
producerSideProtocol1 ProducerHandlers{..} chan =
    awaitOpening >>= awaitOngoing
  where
    awaitOpening = do
      -- The opening message must be this one, to establish the reader state
      MsgSetHead hpoint points <- recvMsg chan
      intersection <- findIntersectionRange hpoint points
      case intersection of
        Just (pt, pt') -> do
          r <- establishReaderState hpoint pt
          sendMsg chan (MsgIntersectImproved pt pt')
          return r
        Nothing -> do
          sendMsg chan MsgIntersectUnchanged
          awaitOpening

    awaitOngoing r = forever $ do
      msg <- recvMsg chan
      case msg of
        MsgRequestNext           -> handleNext r
        MsgSetHead hpoint points -> handleSetHead r hpoint points

    handleNext r = do
      mupdate <- tryReadChainUpdate r
      update  <- case mupdate of
        Just update -> return update

        -- Reader is at the head, have to wait for producer state changes.
        Nothing -> do
          sendMsg chan MsgAwaitReply
          readChainUpdate r
      sendMsg chan (updateMsg update)

    handleSetHead r hpoint points = do
      -- TODO: guard number of points, points sorted
      -- Find the most recent point that is on our chain, and the subsequent
      -- point which is not.
      intersection <- findIntersectionRange hpoint points
      case intersection of
        Just (pt, pt') -> do
          updateReaderState r hpoint (Just pt)
          sendMsg chan (MsgIntersectImproved pt pt')
        Nothing -> do
          updateReaderState r hpoint Nothing
          sendMsg chan MsgIntersectUnchanged

    updateMsg (RollForward  b) = MsgRollForward b
    updateMsg (RollBackward p) = MsgRollBackward p


exampleProducer :: forall m. MonadConc m
                => MVar m (ChainProducerState)
                -> ProducerHandlers m ReaderId
exampleProducer chainvar =
    ProducerHandlers {..}
  where
    findIntersectionRange :: Point -> [Point] -> m (Maybe (Point, Point))
    findIntersectionRange hpoint points = do
      cps <- readMVar chainvar
      return $! findIntersection cps hpoint points

    establishReaderState :: Point -> Point -> m ReaderId
    establishReaderState hpoint ipoint =
      modifyMVar chainvar $ \cps ->
        return $! initialiseReader hpoint ipoint cps

    updateReaderState :: ReaderId -> Point -> Maybe Point -> m ()
    updateReaderState rid hpoint mipoint =
      modifyMVar_ chainvar $ \cps ->
        return $! updateReader rid hpoint mipoint cps

    tryReadChainUpdate :: ReaderId -> m (Maybe (ConsumeChain Block))
    tryReadChainUpdate rid =
      modifyMVar chainvar $ \cps ->
        return $ swizzle cps $ readerInstruction cps rid
      where
        swizzle cps Nothing          = (cps, Nothing)
        swizzle _   (Just (cps', x)) = (cps', Just x)

    readChainUpdate :: ReaderId -> m (ConsumeChain Block)
    readChainUpdate = undefined


--
-- Simulation of composition of producer and consumer
--


-- | Given two sides of a protocol, ...
--
simulateWire
  :: (Chan s p c -> SimM s ())
  -> (Chan s c p -> SimM s ())
  -> SimM s ()
simulateWire protocolSideA protocolSideB = do
    chan <- newChan
    fork $ protocolSideA chan
    fork $ protocolSideB (flipChan chan)
    return ()

