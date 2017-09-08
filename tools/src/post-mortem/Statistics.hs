module Statistics
    ( runJSONFold
    , receivedCreatedF
    , findBlockChainState
    , ChainState(..)
    , ChainStateInternal(..)
    , module Statistics.Block
    , module Statistics.Chart
    , module Statistics.CSV
    , module Statistics.Focus
    , module Statistics.Graph
    , module Statistics.Histogram
    , module Statistics.MemPool
    , module Statistics.Report
    , module Statistics.Throughput
    , module Statistics.Tx
    ) where

import           Control.Foldl   (Fold (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import JSONLog
import Statistics.Block
import Statistics.Chart
import Statistics.CSV
import Statistics.Focus
import Statistics.Graph
import Statistics.Histogram
import Statistics.MemPool
import Statistics.Report
-- import Statistics.Relay
import Statistics.Throughput
import Statistics.Tx
import Types
import Universum
import Util.Pipes    (fold')
import Pos.Util.JsonLog

runJSONFold :: FilePath -> Fold IndexedJLTimedEvent a -> IO a
runJSONFold logDir fd = runParseLogs logDir $ fold' fd

receivedCreatedF :: Fold IndexedJLTimedEvent (Map TxHash (Maybe Timestamp))
receivedCreatedF = f <$> txFirstReceivedF <*> inBlockChainF
  where
    f :: Map TxHash Timestamp -> Map TxHash Timestamp -> Map TxHash (Maybe Timestamp)
    f rm cm = M.mapWithKey g rm
      where
        g :: TxHash -> Timestamp -> Maybe Timestamp
        g tx ts = maybe Nothing (\ts' -> Just $ ts' - ts) $ M.lookup tx cm

type JLSlotId = (Word64, Word16)
type BlockId = Text

data ChainStateInternal = ChainStateInternal
  { stateDescription :: String
  , topMostSlot :: JLSlotId
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
findBlockChainState = Fold f1 (ChainStateInternal "" (0,0) "error" mempty) finish
  where
    finish :: ChainStateInternal -> ChainState
    finish internal = ChainState internal chainLength (((fromIntegral slot) + 1) + ((fromIntegral epoch) * epochLength))
      where
        chainLength = measureLength $ M.lookup (topMostBlock internal) (blocks internal)
        (epoch, slot) = topMostSlot internal
        epochLength = 1080 -- TODO, look it up somehow
    measureLength :: Maybe BlockWrapper -> Integer
    measureLength (Just block) = 1 + (measureLength $ parent block)
    measureLength Nothing = 0
    f1 state1 event = f2 event2
      where
        event2 = ijlEvent event
        f2 (JLCreatedBlock block) = state1 {
          stateDescription = stateDescription state1 <> "\ncreated " <> show block,
          topMostSlot = bestSlot,
          topMostBlock = if (bestSlot == (jlSlot block)) then (jlHash block) else (topMostBlock state1),
          blocks = M.insert (jlHash block) wrapped (blocks state1)
        }
          where
            result = M.lookup (jlPrevBlock block) (blocks state1)
            wrapped = BlockWrapper block result
            bestSlot = max (topMostSlot state1) (jlSlot block)
        f2 (JLAdoptedBlock blockid) = state1
          --stateDescription = (stateDescription state1) <> "\ncadopted " <> (show blockid)
        f2 real_event = state1 { stateDescription = (stateDescription state1) <> "\n" <> (show real_event) }
