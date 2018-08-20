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


--
-- STM based protocol
--

-- | An STM-based interface provided by a chain producer to chain consumers.
--
data ChainProducer = ChainProducer {
       establishChainConsumer :: [Point]
                              -> STM (ChainConsumer, [Point])
     }

data ChainConsumer = ChainConsumer {
       currentReadPoint :: STM Point,
       improveReadPoint :: [Point] -> STM (),
       tryPeekChain     :: STM (Maybe (ConsumeChain Block)),
       tryReadChain     :: STM (Maybe (ConsumeChain Block))
     }

type MaxReadBlocks = Int

readRollForwardOnly :: ChainConsumer -> MaxReadBlocks -> STM [Block]
readRollForwardOnly ChainConsumer{tryPeekChain, tryReadChain} maxBlocks =
    go maxBlocks
  where 
    go 0 = return []
    go n = do
      res <- tryPeekChain
      case res of
        Just (RollForward b) -> do
          _ <- tryReadChain
          bs <- go (n-1)
          return (b:bs)
        _ -> return []

-- | Like 'tryReadChain' but reads multiple blocks in one go.
--
tryReadChainN :: ChainConsumer
              -> MaxReadBlocks -- ^ The maximum number of blocks to read
              -> STM (Maybe (ConsumeChain [Block]))
tryReadChainN cs@ChainConsumer{..} maxBlocks = do
    res <- tryReadChain
    case res of
      -- If we're at the chain head or it's a rollback we just return that.
      Nothing               -> return Nothing
      Just (RollBackward p) -> return (Just (RollBackward p))
      -- If we get one block we peek at what's ahead and consume any
      -- more blocks, up to our limit.
      Just (RollForward b) -> do
        bs <- readRollForwardOnly cs (maxBlocks-1)
        return (Just (RollForward (b:bs)))

-- | Like 'tryReadChainN' but blocks at the chain head.
--
readChainN :: ChainConsumer
           -> MaxReadBlocks -- ^ The maximum number of blocks to read
           -> STM (ConsumeChain [Block])
readChainN cs@ChainConsumer{..} maxBlocks = do
    res <- tryReadChain
    case res of
      -- If it's the chain head we block by retrying.
      Nothing                 -> retry
      -- If it's a rollback we just return that.
      Just (RollBackward p) -> return (RollBackward p)
      -- If we get one block we peek at what's ahead and consume any
      -- more blocks, up to our limit.
      Just (RollForward b) -> do
        bs <- readRollForwardOnly cs (maxBlocks-1)
        return (RollForward (b:bs))

