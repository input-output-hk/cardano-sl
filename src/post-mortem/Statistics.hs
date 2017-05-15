module Statistics
    ( foldTxData
    , runFoldTxData
    , transformTxData
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import           Pipes
import qualified Pipes.Prelude   as P

import JSONLog
import Pos.Util.JsonLog          (JLEvent (..), JLTxR (..), JLBlock (..))
import Prelude                   (id)
import Universum

data TxData = TxData
    { txdReceived :: !(Map Text [(Int, Integer)])
    , txdBlocks   :: !(Map Text [(Int, Integer)])
    } deriving Show

foldTxData :: Producer IndexedJLTimedEvent IO () -> IO TxData
foldTxData p = P.fold step (TxData M.empty M.empty) id p
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

runFoldTxData :: FilePath -> IO TxData
runFoldTxData logDir = runParseLogs logDir foldTxData

transformTxData :: ([(Int, Integer)] -> [(Int, Integer)] -> a) -> TxData -> Map Text a
transformTxData f TxData{..} =
    let keys = S.toList $ S.fromList (M.keys txdReceived) `S.union` S.fromList (M.keys txdBlocks)
    in  M.fromList [(k, f (M.findWithDefault [] k txdReceived)
                          (M.findWithDefault [] k txdBlocks)) | k <- keys]
