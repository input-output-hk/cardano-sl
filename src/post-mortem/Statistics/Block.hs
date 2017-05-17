module Statistics.Block
    ( BlockHeader (..)
    , blockHeadersF
    , blockChain
    , blockChainF
    , txBlocksF
    , inBlockChainF
    , txCntInChainF
    ) where

import           Control.Foldl    (Fold (..))
import qualified Data.Map.Strict  as MS
import qualified Data.Map.Lazy    as ML
import           Data.Maybe       (fromJust, isJust)
import qualified Data.Set         as S
import qualified Data.Text        as T

import           JSONLog          (IndexedJLTimedEvent (..))
import           Pos.Util.JsonLog (JLEvent (..), JLBlock (..))
import           Prelude          (id)
import           Types
import           Universum

data BlockHeader = BlockHeader
    { bhNode      :: !NodeIndex
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

txBlocksF :: Fold IndexedJLTimedEvent (SMap Text [(Integer, Text)])
txBlocksF = Fold step MS.empty id
  where
    step :: SMap Text [(Integer, Text)] -> IndexedJLTimedEvent -> SMap Text [(Integer, Text)]
    step m IndexedJLTimedEvent{..} = case ijlEvent of
        JLCreatedBlock JLBlock{..} -> foldl' (f ijlTimestamp jlHash) m [T.take 8 x | x <- jlTxs]
        _                          -> m

    f :: Integer -> Text -> SMap Text [(Integer, Text)] -> Text -> SMap Text [(Integer, Text)]
    f ts h m tx = let y = (ts, h)
                  in  MS.alter (Just . maybe [y] (y :)) tx m

inBlockChainF :: Fold IndexedJLTimedEvent (SMap TxHash Timestamp)
inBlockChainF = f <$> txBlocksF <*> blockChainF
  where
    f :: SMap TxHash [(Timestamp, BlockHash)] -> Set BlockHash -> SMap TxHash Timestamp
    f m chain = mapMaybe' g m
      where
        g :: [(Timestamp, BlockHash)] -> Maybe Timestamp
        g xs = case [ts | (ts, h) <- xs, S.member h chain] of
            []  -> Nothing
            tss -> Just $ minimum tss

        mapMaybe' :: (a -> Maybe b) -> SMap k a -> SMap k b
        mapMaybe' h = MS.map fromJust . MS.filter isJust . MS.map h

txCntInChainF :: Fold IndexedJLTimedEvent [(NodeIndex, Timestamp, Int)]
txCntInChainF = f <$> blockHeadersF <*> blockChainF
  where
    f :: Map BlockHash BlockHeader -> Set BlockHash -> [(NodeIndex, Timestamp, Int)]
    f m cs = [(bhNode, bhTimestamp, bhTxCnt) | (_, BlockHeader{..}) <- MS.toList m, bhHash `S.member` cs]
