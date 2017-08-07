module Statistics.Focus
    ( Focus (..)
    , focusF
    ) where

import           Control.Foldl   (Fold (..))
import           Data.Set        (Set)
import qualified Data.Set        as S
import qualified Data.Text       as T

import JSONLog
import Pos.Util.JsonLog          (JLEvent (..), JLTxR (..), JLBlock(..))
import Prelude                   (id)
import Types
import Universum

data Focus =
      Received !(Maybe Text)
    | InCreatedBlock !BlockHash
    | InAdoptedBlock !BlockHash
    deriving Show

focusF :: TxHash -> Fold IndexedJLTimedEvent [(Timestamp, NodeIndex, Focus)]
focusF tx = f <$> allF <*> blocksF
  where
    f :: [(Timestamp, NodeIndex, Focus)] -> Set BlockHash -> [(Timestamp, NodeIndex, Focus)]
    f xs s = filter g xs
      where
        g :: (Timestamp, NodeIndex, Focus) -> Bool
        g (_, _, x) = case x of
            (Received _)       -> True
            (InCreatedBlock h) -> S.member h s
            (InAdoptedBlock h) -> S.member h s

    allF :: Fold IndexedJLTimedEvent [(Timestamp, NodeIndex, Focus)]
    allF = reverse <$> Fold step [] id
      where
        step :: [(Timestamp, NodeIndex, Focus)] -> IndexedJLTimedEvent -> [(Timestamp, NodeIndex, Focus)]
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
            (JLCreatedBlock JLBlock{..}) -> if tx `elem` [T.take 8 tx' | tx' <- jlTxs]
                                                then S.insert jlHash s
                                                else s
            _                            ->  s
