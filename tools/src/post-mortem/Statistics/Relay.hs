module Statistics.Relay
    ( relayQueueFullF
    , relayEnqueueDequeueTimeF
    ) where

import           Control.Foldl           (Fold (..))
import           Data.Time.Units         (Microsecond)

import           JSONLog                 (IndexedJLTimedEvent (..))
import           Pos.Communication.Types (RelayLogEvent (..))
import           Pos.Util.JsonLog        (JLEvent (..))
import           Types
import           Universum

relayQueueFullF :: Fold IndexedJLTimedEvent [(NodeIndex, Timestamp)]
relayQueueFullF = Fold step [] reverse
  where
    step :: [(NodeIndex, Timestamp)] -> IndexedJLTimedEvent -> [(NodeIndex, Timestamp)]
    step ts IndexedJLTimedEvent{..} = case ijlEvent of
        JLRelayEvent RelayQueueFull -> (ijlNode, ijlTimestamp) : ts
        _                           -> ts

relayEnqueueDequeueTimeF :: Fold IndexedJLTimedEvent [(NodeIndex, Timestamp, Microsecond)]
relayEnqueueDequeueTimeF = Fold step [] reverse
  where
    step :: [(NodeIndex, Timestamp, Microsecond)]
         -> IndexedJLTimedEvent
         -> [(NodeIndex, Timestamp, Microsecond)]
    step ts IndexedJLTimedEvent{..} = case ijlEvent of
        JLRelayEvent (EnqueueDequeueTime s) -> (ijlNode, ijlTimestamp, fromIntegral s) : ts
        _                                   -> ts
