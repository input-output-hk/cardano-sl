module Statistics.Tx
    ( txReceivedF
    ) where

import           Control.Foldl   (Fold (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import JSONLog
import Pos.Util.JsonLog          (JLEvent (..), JLTxR (..))
import Prelude                   (id)
import Types
import Universum

txReceivedF :: Fold IndexedJLTimedEvent (Map TxHash Timestamp)
txReceivedF = Fold step M.empty id
  where
    step :: Map TxHash Timestamp -> IndexedJLTimedEvent -> Map TxHash Timestamp
    step m IndexedJLTimedEvent{..} = case ijlEvent of
        JLTxReceived JLTxR{..} -> M.alter (f ijlTimestamp) jlrTxId m
        _                      -> m

    f :: Timestamp -> Maybe Timestamp -> Maybe Timestamp
    f ts Nothing    = Just ts
    f ts (Just ts') = Just $ min ts ts'
