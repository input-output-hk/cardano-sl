module Statistics.HydraMetrics
    ( findBlockChainState
    , ChainState(..)
    ) where

import           Control.Foldl   (Fold (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           JSONLog         (IndexedJLTimedEvent, IndexedJLTimedEvent(..))
import           Pos.Util.JsonLog(JLBlock, JLBlock(..),  JLEvent(..))
import           Universum

type JLSlotId = (Word64, Word16)
type BlockId = Text

data ChainStateInternal = ChainStateInternal
  { topMostSlot :: JLSlotId
  , topMostBlock :: BlockId
  , blocks :: Map BlockId BlockWrapper
  } deriving Show

data ChainState = ChainState
  { internal :: ChainStateInternal
  , chainLength :: Integer
  , expectedLength :: Integer
  } deriving Show

data BlockWrapper = BlockWrapper
  { jlBlock :: JLBlock
  , parent :: Maybe BlockWrapper
  } deriving Show

findBlockChainState :: Fold IndexedJLTimedEvent ChainState
findBlockChainState = Fold combine (ChainStateInternal (0,0) "error" mempty) finish
  where
    finish :: ChainStateInternal -> ChainState
    finish internal = ChainState internal chainLength (((fromIntegral slot) + 1) + ((fromIntegral epoch) * epochLength))
      where
        chainLength = measureLength $ M.lookup (topMostBlock internal) (blocks internal)
        (epoch, slot) = topMostSlot internal
        epochLength = 1080 -- TODO, look it up somehow
    measureLength :: Maybe BlockWrapper -> Integer -- TODO, make this tail-recursive
    measureLength (Just block) = 1 + measureLength (parent block)
    measureLength Nothing = 0
    combine :: ChainStateInternal -> IndexedJLTimedEvent -> ChainStateInternal
    combine state1 event = case ijlEvent event of
      (JLCreatedBlock block) -> f2 block
      _ -> state1
      where
        f2 block = state1 {
          topMostSlot = bestSlot,
          topMostBlock = if bestSlot == jlSlot block then jlHash block else topMostBlock state1,
          blocks = M.insert (jlHash block) wrapped (blocks state1)
        }
          where
            result = M.lookup (jlPrevBlock block) (blocks state1)
            wrapped = BlockWrapper block result
            bestSlot = max (topMostSlot state1) (jlSlot block)
