module Statistics.Block
    ( blockHeadersF
    , blockChain
    , txBlocksF
    ) where

import           Control.Foldl    (Fold (..))
import qualified Data.Map.Strict  as MS
import qualified Data.Map.Lazy    as ML
import qualified Data.Text        as T

import           JSONLog          (IndexedJLTimedEvent (..))
import           Pos.Util.JsonLog (JLEvent (..), JLBlock (..))
import           Prelude          (id)
import           Universum

data BlockHeader = BlockHeader
    { bhNode      :: !Int
    , bhTimestamp :: !Integer
    , bhHash      :: !Text
    , bhPrevBlock :: !Text
    , bhSlot      :: !(Word64, Word16)
    } deriving Show

type SMap k a = MS.Map k a
type LMap k a = ML.Map k a

blockHeadersF :: Fold IndexedJLTimedEvent (SMap Text BlockHeader)
blockHeadersF = Fold step MS.empty id
  where
    step :: SMap Text BlockHeader -> IndexedJLTimedEvent -> SMap Text BlockHeader
    step m IndexedJLTimedEvent{..} = case ijlEvent of
        JLCreatedBlock JLBlock{..} -> 
            let bh = BlockHeader
                    { bhNode      = ijlNodeIndex
                    , bhTimestamp = ijlTimestamp
                    , bhHash      = jlHash
                    , bhPrevBlock = jlPrevBlock
                    , bhSlot      = jlSlot
                    }
            in MS.insert jlHash bh m

        _                          -> m

blockChain :: SMap Text BlockHeader -> [Text]
blockChain m = maybe [] id $ head $ sortByLengthDesc [getLongestChain h | h <- allHashes]
  where
    allHashes :: [Text]
    allHashes = MS.keys m

    sortByLengthDesc :: [[a]] -> [[a]]
    sortByLengthDesc = sortBy (compare `on` (negate . length))

    successors :: SMap Text [Text]
    successors = foldl' f MS.empty allHashes

    getSuccessors :: Text -> [Text]
    getSuccessors h = MS.findWithDefault [] h successors

    f :: SMap Text [Text] -> Text -> SMap Text [Text]
    f s h = let BlockHeader{..} = m MS.! h
            in  MS.alter (Just . maybe [bhHash] (bhHash :)) bhPrevBlock s

    longestChains :: LMap Text [Text]
    longestChains = ML.fromList [(h, getLongestChain h) | h <- allHashes]

    getLongestChain :: Text -> [Text]
    getLongestChain h = case sortByLengthDesc $ map (longestChains ML.!) $ getSuccessors h of
        []      -> [h]
        (c : _) -> h : c

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
