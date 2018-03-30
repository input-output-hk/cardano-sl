module Statistics.Focus
    ( Focus (..)
    , focusF
    ) where

import           Control.Foldl (Fold (..))
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           JSONLog
import           Pos.Util.JsonLog.Events (JLBlock (..), JLEvent (..), JLTxR (..))
import           Prelude (id)
import           Types
import           Universum

data Focus =
      Received !(Maybe Text)
    | InCreatedBlock !BlockHash
    | InAdoptedBlock !BlockHash
    deriving Show

focusF :: TxHash -> Fold IndexedJLTimedEvent [(Timestamp, NodeId, Focus)]
focusF tx = f <$> allF <*> blocksF
  where
    f :: [(Timestamp, NodeId, Focus)] -> Set BlockHash -> [(Timestamp, NodeId, Focus)]
    f xs s = filter g xs
      where
        g :: (Timestamp, NodeId, Focus) -> Bool
        g (_, _, x) = case x of
            (Received _)       -> True
            (InCreatedBlock h) -> S.member h s
            (InAdoptedBlock h) -> S.member h s

    allF :: Fold IndexedJLTimedEvent [(Timestamp, NodeId, Focus)]
    allF = reverse <$> Fold step [] id
      where
        step :: [(Timestamp, NodeId, Focus)] -> IndexedJLTimedEvent -> [(Timestamp, NodeId, Focus)]
        step xs IndexedJLTimedEvent{..} = case ijlEvent of
            (JLCreatedBlock JLBlock{..}) -> (ijlTimestamp, ijlNode, InCreatedBlock jlHash) : xs
            (JLAdoptedBlock h)           -> (ijlTimestamp, ijlNode, InAdoptedBlock h)      : xs
            (JLTxReceived JLTxR{..})     -> if jlrTxId == tx
                                                then (ijlTimestamp, ijlNode, Received jlrError) : xs
                                                else xs
            _                            -> xs

    blocksF :: Fold IndexedJLTimedEvent (Set BlockHash)
    blocksF = Fold step S.empty id
      where
        step :: Set BlockHash -> IndexedJLTimedEvent -> Set BlockHash
        step s IndexedJLTimedEvent{..} = case ijlEvent of
            (JLCreatedBlock JLBlock{..}) -> if tx `elem` [T.take 16 tx' | tx' <- jlTxs]
                                                then S.insert jlHash s
                                                else s
            _                            ->  s
