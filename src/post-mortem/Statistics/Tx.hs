module Statistics.Tx
    ( txReceivedF
    ) where

import           Control.Foldl   (Fold (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import JSONLog
import Pos.Util.JsonLog          (JLEvent (..), JLTxR (..))
import Prelude                   (id)
import Universum

txReceivedF :: Fold IndexedJLTimedEvent (Map Text Integer)
txReceivedF = Fold step M.empty id
  where
    step :: Map Text Integer -> IndexedJLTimedEvent -> Map Text Integer
    step m IndexedJLTimedEvent{..} = case ijlEvent of
        JLTxReceived JLTxR{..} -> M.alter (f ijlTimestamp) jlrTxId m
        _                      -> m

    f :: Integer -> Maybe Integer -> Maybe Integer
    f ts Nothing    = Just ts
    f ts (Just ts') = Just $ min ts ts'
