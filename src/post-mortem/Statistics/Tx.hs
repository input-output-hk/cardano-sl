module Statistics.Tx
    ( TxData (..)
    , txDataF
    , transformTxData
    ) where

import           Control.Foldl   (Fold (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T

import JSONLog
import Pos.Util.JsonLog          (JLEvent (..), JLTxR (..), JLBlock (..))
import Prelude                   (id)
import Universum

data TxData = TxData
    { txdReceived :: !(Map Text [(Int, Integer)])
    , txdBlocks   :: !(Map Text [(Int, Integer)])
    } deriving Show

txDataF :: Fold IndexedJLTimedEvent TxData
txDataF = Fold step (TxData M.empty M.empty) id
  where
    step :: TxData -> IndexedJLTimedEvent -> TxData
    step d@TxData{..} IndexedJLTimedEvent{..} = case ijlEvent of
        JLTxReceived JLTxR{..}     -> d { txdReceived = M.alter (f (ijlNodeIndex, jlrReceived)) jlrTxId txdReceived }
        JLCreatedBlock JLBlock{..} -> d { txdBlocks   = foldl' (g ijlNodeIndex ijlTimestamp) txdBlocks jlTxs }
        _                          -> d

    f :: a -> Maybe [a] -> Maybe [a]
    f x = maybe (Just [x]) (Just . (x :))

    g :: Int -> Integer -> Map Text [(Int, Integer)] -> Text -> Map Text [(Int, Integer)]
    g n t m x = M.alter (f (n, t)) (T.take 8 x) m

transformTxData :: ([(Int, Integer)] -> [(Int, Integer)] -> a) -> TxData -> Map Text a
transformTxData f TxData{..} =
    let keys = S.toList $ S.fromList (M.keys txdReceived) `S.union` S.fromList (M.keys txdBlocks)
    in  M.fromList [(k, f (M.findWithDefault [] k txdReceived)
                          (M.findWithDefault [] k txdBlocks)) | k <- keys]
