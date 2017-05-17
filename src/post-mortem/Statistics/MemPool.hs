module Statistics.MemPool
    ( memPoolF
    ) where

import           Control.Foldl    (Fold (..))

import           JSONLog          (IndexedJLTimedEvent (..))
import           Pos.Util.JsonLog (JLEvent (..), JLMemPool (..))
import           Prelude          (id)
import           Types
import           Universum

memPoolF :: Fold IndexedJLTimedEvent [(NodeIndex, Timestamp, Int)]
memPoolF = Fold step [] id
  where
    step :: [(NodeIndex, Timestamp, Int)] -> IndexedJLTimedEvent -> [(NodeIndex, Timestamp, Int)]
    step xs IndexedJLTimedEvent{..} = case ijlEvent of
        JLMemPoolEvent (JLMemPool {..}) -> (ijlNode, ijlTimestamp, jlmSizeAfter) : xs
        _               -> xs
