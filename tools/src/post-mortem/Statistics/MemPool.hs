module Statistics.MemPool
    ( memPoolF
    ) where

import           Control.Foldl (Fold (..))

import           JSONLog (IndexedJLTimedEvent (..))
import           Pos.Util.JsonLog.Events (JLEvent (..), JLMemPool (..))
import           Prelude (id)
import           Types

memPoolF :: Fold IndexedJLTimedEvent [(NodeId, Timestamp, JLMemPool)]
memPoolF = Fold step [] id
  where
    step :: [(NodeId, Timestamp, JLMemPool)]
         -> IndexedJLTimedEvent
         -> [(NodeId, Timestamp, JLMemPool)]
    step xs IndexedJLTimedEvent{..} = case ijlEvent of
        JLMemPoolEvent mp -> (ijlNode, ijlTimestamp, mp) : xs
        _                 -> xs

{-
memPoolF :: Fold IndexedJLTimedEvent [(NodeId, Timestamp, Int)]
memPoolF = Fold step [] id
  where
    step :: [(NodeId, Timestamp, Int)] -> IndexedJLTimedEvent -> [(NodeId, Timestamp, Int)]
    step xs IndexedJLTimedEvent{..} = case ijlEvent of
        JLMemPoolEvent (JLMemPool {..}) -> (ijlNode, ijlTimestamp, jlmSizeAfter) : xs
        _                               -> xs
        -}
