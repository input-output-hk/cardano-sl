module Statistics.Tx
    ( txReceivedF
    , txFirstReceivedF
    ) where

import           Control.Foldl   (Fold (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import JSONLog
import Pos.Util.JsonLog          (JLEvent (..), JLTxR (..))
import Prelude                   (id, head)
import Types
import Universum                 hiding (head)

txReceivedF :: Fold IndexedJLTimedEvent (Map TxHash [(Timestamp, NodeIndex)])
txReceivedF = M.map reverse <$> Fold step M.empty id
  where
    step :: Map TxHash [(Timestamp, NodeIndex)] -> IndexedJLTimedEvent -> Map TxHash [(Timestamp, NodeIndex)]
    step m IndexedJLTimedEvent{..} = case ijlEvent of
        JLTxReceived JLTxR{..} -> M.insertWith (++) jlrTxId [(ijlTimestamp, ijlNode)] m
        _                      -> m

txFirstReceivedF :: Fold IndexedJLTimedEvent (Map TxHash Timestamp)
txFirstReceivedF = M.map (fst . head) <$> txReceivedF
