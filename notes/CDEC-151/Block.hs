{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Block
    ( Block (..)
    , BlockId
    , Slot
    , ChainMeasure (..)
    , hashBlock
    , mkBlock
    , genBlock
    , genNBlocks
    , ReaderId
    , ReaderState (..)
    , ReaderStates
    , Point
    , blockPoint
    )
    where

import           Data.FingerTree (Measured (..))
import           Data.Hashable
import           Test.QuickCheck

data Block = Block {
       blockId      :: BlockId,  -- ^ hash of other fields
       prevBlockId  :: BlockId,  -- ^ 'blockId' of the previous block
       blockSlot    :: Slot,
       blockPayload :: Payload
     }
  deriving (Show, Eq)

data ChainMeasure = ChainMeasure {
       minSlot :: !Slot,
       maxSlot :: !Slot,
       size    :: !Int
     }
  deriving Show

instance Monoid ChainMeasure where
  mempty = ChainMeasure maxBound minBound 0
  mappend vl vr =
    ChainMeasure (min (minSlot vl) (minSlot vr))
                 (max (maxSlot vl) (maxSlot vr))
                 (size vl + size vr)

instance Measured ChainMeasure Block where
  measure Block{blockSlot} = ChainMeasure blockSlot blockSlot 1

type BlockId = Int
type Slot    = Word
type Payload = String

hashBlock :: Block -> BlockId
hashBlock Block{prevBlockId, blockSlot, blockPayload} =
    hash (prevBlockId, blockSlot, blockPayload)

--
-- Generating valid chains
--

mkBlock :: BlockId -> Slot -> Payload -> Block
mkBlock blockid' slot payload = block
  where
    block   = Block blockid blockid' slot payload
    blockid = hashBlock block

genBlock :: BlockId -> Slot -> Gen Block
genBlock blockid slot = do
    payload <- vectorOf 4 (choose ('A', 'Z'))
    return (mkBlock blockid slot payload)

genNBlocks :: Int -> BlockId -> Slot -> Gen [Block]
genNBlocks 1 blockid0 slot0 = (:[]) <$> genBlock blockid0 slot0
genNBlocks n blockid0 slot0 = do
    c@(b':_) <- genNBlocks (n-1) blockid0 slot0
    b        <- genBlock (blockId b') (blockSlot b' + 1)
    return (b:c)


-- | Readers are represented here as a relation.
--
type ReaderStates = [ReaderState]

-- | A point on the chain is identified by the 'Slot' number and its 'BlockId'.
-- The 'Slot' tells us where to look and the 'BlockId' either simply serves as
-- a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
type Point        = (Slot, BlockId)
type ReaderId     = Int
data ReaderState  = ReaderState {
       -- | Where the chain of the consumer and producer intersect. If the
       -- consumer is on the chain then this is the same as the 'readerHead',
       -- but if the consumer 'readerHead' is off the chain then this is the
       -- point the consumer will need to rollback to.
       readerIntersection :: Point,

       -- | Where the chain consumer was last reading from (typically the
       -- head of the consumer's chain). If this is on the producer chain
       -- then it is equal to the 'readerIntersection'.
       readerHead         :: Point,

       -- | A unique tag per reader, to distinguish different readers.
       readerId           :: ReaderId
     }
  deriving (Eq, Show)

blockPoint :: Block -> Point
blockPoint b = (blockSlot b, blockId b)
