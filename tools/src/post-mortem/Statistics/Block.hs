module Statistics.Block
    ( BlockHeader (..)
    , blockHeadersF
    , blockChain
    , blockChainF
    , txBlocksF
    , inBlockChainF
    , txCntInChainF
    , TxFate (..)
    , txFateF
    ) where

import           Control.Foldl (Fold (..), fold)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as MS
import           Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time.Units (Microsecond)

import           JSONLog (IndexedJLTimedEvent (..))
import           Pos.Util.JsonLog.Events (JLBlock (..), JLEvent (..))
import           Prelude (id)
import           Statistics.Tx (txFirstReceivedF)
import           Types
import           Universum hiding (fold)

data BlockHeader = BlockHeader
    { bhNode      :: !NodeId
    , bhTimestamp :: !Timestamp
    , bhHash      :: !BlockHash
    , bhPrevBlock :: !BlockHash
    , bhSlot      :: !Slot
    , bhTxCnt     :: !Int
    } deriving Show

type SMap k a = MS.Map k a
type LMap k a = ML.Map k a

blockHeadersF :: Fold IndexedJLTimedEvent (SMap BlockHash BlockHeader)
blockHeadersF = Fold step MS.empty id
  where
    step :: SMap Text BlockHeader -> IndexedJLTimedEvent -> SMap Text BlockHeader
    step m IndexedJLTimedEvent{..} = case ijlEvent of
        JLCreatedBlock JLBlock{..} ->
            let bh = BlockHeader
                    { bhNode      = ijlNode
                    , bhTimestamp = ijlTimestamp
                    , bhHash      = jlHash
                    , bhPrevBlock = jlPrevBlock
                    , bhSlot      = jlSlot
                    , bhTxCnt     = length jlTxs
                    }
            in MS.insert jlHash bh m

        _                          -> m

blockChain :: SMap BlockHash BlockHeader -> Set BlockHash
blockChain m = S.fromList $ maybe [] id $ head $ sortByLengthDesc [getLongestChain h | h <- allHashes]
  where
    allHashes :: [BlockHash]
    allHashes = MS.keys m

    sortByLengthDesc :: [[a]] -> [[a]]
    sortByLengthDesc = sortBy (compare `on` (negate . length))

    successors :: SMap BlockHash [BlockHash]
    successors = foldl' f MS.empty allHashes

    getSuccessors :: BlockHash -> [BlockHash]
    getSuccessors h = MS.findWithDefault [] h successors

    f :: SMap BlockHash [BlockHash] -> BlockHash -> SMap BlockHash [BlockHash]
    f s h = let BlockHeader{..} = m MS.! h
            in  MS.alter (Just . maybe [bhHash] (bhHash :)) bhPrevBlock s

    longestChains :: LMap Text [Text]
    longestChains = ML.fromList [(h, getLongestChain h) | h <- allHashes]

    getLongestChain :: Text -> [Text]
    getLongestChain h = case sortByLengthDesc $ map (longestChains ML.!) $ getSuccessors h of
        []      -> [h]
        (c : _) -> h : c

blockChainF :: Fold IndexedJLTimedEvent (Set BlockHash)
blockChainF = blockChain <$> blockHeadersF

txBlocksF :: Fold IndexedJLTimedEvent (SMap TxHash [(Timestamp, BlockHash)])
txBlocksF = Fold step MS.empty id
  where
    step :: SMap Text [(Microsecond, Text)]
         -> IndexedJLTimedEvent
         -> SMap Text [(Microsecond, Text)]
    step m IndexedJLTimedEvent{..} = case ijlEvent of
        JLCreatedBlock JLBlock{..} -> foldl' (f ijlTimestamp jlHash) m [T.take 16 x | x <- jlTxs]
        _                          -> m

    f :: Timestamp
      -> Text
      -> SMap Text [(Timestamp, Text)]
      -> Text
      -> SMap Text [(Timestamp, Text)]
    f ts h m tx = let y = (ts, h)
                  in  MS.alter (Just . maybe [y] (y :)) tx m

inBlockChainF :: Fold IndexedJLTimedEvent (SMap TxHash Timestamp)
inBlockChainF = f <$> txFateF
  where
    f :: SMap TxHash TxFate -> SMap TxHash Timestamp
    f = MS.map fromJust . MS.filter isJust .  MS.map txInBlockChain

txCntInChainF :: Fold IndexedJLTimedEvent [(NodeId, Timestamp, Int)]
txCntInChainF = f <$> blockHeadersF <*> blockChainF
  where
    f :: Map BlockHash BlockHeader -> Set BlockHash -> [(NodeId, Timestamp, Int)]
    f m cs = [(bhNode, bhTimestamp, bhTxCnt) | (_, BlockHeader{..}) <- MS.toList m, bhHash `S.member` cs]

data TxFate =
      InNoBlock
    | InBlockChain !Timestamp !BlockHash !(Set (Timestamp, BlockHash))
    | InFork !(Set (Timestamp, BlockHash))
    deriving Show

txInBlockChain :: TxFate -> Maybe Timestamp
txInBlockChain InNoBlock             = Nothing
txInBlockChain (InBlockChain ts _ _) = Just ts
txInBlockChain (InFork _)            = Nothing

txFateF :: Fold IndexedJLTimedEvent (SMap TxHash TxFate)
txFateF = f <$> txFirstReceivedF <*> txBlocksF <*> blockChainF
  where
    f :: SMap TxHash Timestamp -> SMap TxHash [(Timestamp, BlockHash)] -> Set BlockHash -> SMap TxHash TxFate
    f received blocks chain = MS.fromList [(tx, fate tx) | tx <- MS.keys received]
      where
        fate :: TxHash -> TxFate
        fate tx = case MS.lookup tx blocks of
            Nothing -> InNoBlock
            Just xs -> fold (Fold step (Nothing, S.empty) extract) xs

        step :: (Maybe (Timestamp, BlockHash), Set (Timestamp, BlockHash))
             -> (Timestamp, BlockHash)
             -> (Maybe (Timestamp, BlockHash), Set (Timestamp, BlockHash))
        step (Nothing, s) (ts, h)
            | S.member h chain = (Just (ts, h), s)
            | otherwise        = (Nothing     , S.insert (ts, h) s)
        step (p      , s) q    = (p           , S.insert q       s)

        extract :: (Maybe (Timestamp, BlockHash), Set (Timestamp, BlockHash)) -> TxFate
        extract (Nothing     , s) = InFork s
        extract (Just (ts, h), s) = InBlockChain ts h s
