{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module ChainExperiment2 where

import           Data.List (find)

import           Block (Block (..), Point, ReaderId, ReaderState (..), ReaderStates, blockPoint,
                        genBlock)
import           Chain (Chain, absChainFragment, applyChainUpdate, invChain, pointOnChain,
                        validChain)
import qualified Chain
import qualified Chain.Abstract as Chain.Abs
import           Chain.Update (ChainUpdate (..))

-- import Control.Applicative
import           Control.Exception (assert)

import           Test.QuickCheck

validChainUpdate :: ChainUpdate -> Chain -> Bool
validChainUpdate cu c = validChain (applyChainUpdate cu c)

k :: Int
k = 5 -- maximum fork length in these tests

--
-- Generating valid chain updates
--

genChainUpdate :: Chain.Abs.Chain -> Gen ChainUpdate
genChainUpdate chain = do
    let maxRollback = length (take k chain)
    n <- choose (-10, maxRollback)
    if n <= 0
      then AddBlock <$> genBlock (Chain.Abs.chainHeadBlockId chain)
                                 (Chain.Abs.chainHeadSlot chain + 1)
      else return $ RollBack (blockPoint (head (drop (n - 1) chain)))

genChainUpdates :: Chain.Abs.Chain -> Int -> Gen [ChainUpdate]
genChainUpdates _     0 = return []
genChainUpdates chain n = do
    update  <- genChainUpdate chain
    let chain' = Chain.Abs.applyChainUpdate update chain
    updates <- genChainUpdates chain' (n-1)
    return (update : updates)

data TestChainAndUpdates = TestChainAndUpdates Chain.Abs.Chain [ChainUpdate]
  deriving Show

instance Arbitrary TestChainAndUpdates where
  arbitrary = do
    (Positive n, Positive m) <- arbitrary
    chain   <- Chain.Abs.genChain n
    updates <- genChainUpdates chain m
    return (TestChainAndUpdates chain updates)

prop_TestChainAndUpdates :: TestChainAndUpdates -> Bool
prop_TestChainAndUpdates (TestChainAndUpdates chain updates) =
    all validChain chains
 && all (uncurry validChainUpdate) (zip updates chains)
  where
    chains = scanl (flip applyChainUpdate) (Chain.reifyChainFragment chain) updates

-- | This is now the simulation property covering both the add block and
-- switch fork operations.
--
prop_switchFork :: TestChainAndUpdates -> Bool
prop_switchFork (TestChainAndUpdates chain updates) =
    all invChain chains'
    && all (\(c, c') -> c == absChainFragment c') (zip chains chains')
  where
    chains' = scanl
        (flip Chain.applyChainUpdate)
        (Chain.reifyChainFragment chain)
        updates
    chains  = scanl
        (flip Chain.Abs.applyChainUpdate)
        chain
        updates

--
-- Step 4: switching forks again! Roll back and roll forward.
--

-- For the chain following protocol we will need to relax the constraint that
-- we always switch fork all in one go (and to a longer chain), and have to
-- break it up into a rollback followed by adding more blocks.
--
-- Furthermore, it turns out for the chain following protocol that it works
-- better to specify the rollback in terms of where to roll back to, rather
-- than on how many blocks to roll back.

--rollBackToVolatile :: Point -> Volatile -> Volatile
--rollBackToVolatile _ (Volatile _ Nothing) =
--    error "rollBackToVolatile: precondition violation"

--rollBackToVolatile (slot, bid) (Volatile blocks (Just tip)) =


--
-- Read pointer operations
--

-- A 'ChainState' plus an associated set of readers/consumers of the chain.

data ChainProducerState rep = ChainProducerState {
       chainState   :: rep,
       chainReaders :: ReaderStates
     }

-- | Readers are represented here as a relation.
invChainProducerState :: ChainProducerState Chain -> Bool
invChainProducerState (ChainProducerState cs rs) =
    invChain cs
 && Chain.invReaderStates cs rs


{-
Hmm, perhaps this version does too much, lets simplify

initialiseReadPointer :: [Point]
                      -> ChainState
                      -> Maybe (ChainState, ReadPointer)
initialiseReadPointer checkpoints (ChainState v rs) = do
    (c, c') <- findIntersectionRange checkpoints
    let rs' = (c, readPtr, ) : rs
    return (ChainState v rs')
  where
    readPtr = freshReaderId rs

    findIntersectionRange cs =
      find (checkpointOnChain . fst)
           (zip cs (head cs ++ cs))

-}

-- Given a list of points, find the most recent pair such that older is on the
-- chain and the newer is not.
--
-- > [x, x'] `subseq` xs, not (onChain x), onChain x'
--


initialiseReader :: Point
                 -> Point
                 -> ChainProducerState Chain
                 -> (ChainProducerState Chain, ReaderId)
initialiseReader hpoint ipoint (ChainProducerState cs rs) =
    assert (pointOnChain cs ipoint) $
    (ChainProducerState cs (r:rs), readerId r)
  where
    r = ReaderState {
          readerIntersection = ipoint,
          readerHead         = hpoint,
          readerId           = freshReaderId rs
        }

freshReaderId :: ReaderStates -> ReaderId
freshReaderId rs = 1 + maximum [ readerId | ReaderState{readerId} <- rs ]

updateReader :: ReaderId
             -> Point
             -> Maybe Point
             -> ChainProducerState Chain
             -> ChainProducerState Chain
updateReader rid hpoint mipoint (ChainProducerState cs rs) =
    ChainProducerState cs [ if readerId r == rid then update r else r
                          | r <- rs ]
  where
    update r = case mipoint of
      Nothing     -> r { readerHead = hpoint }
      Just ipoint -> assert (pointOnChain cs ipoint) $
                     r {
                       readerHead         = hpoint,
                       readerIntersection = ipoint
                     }

lookupReader :: ChainProducerState rep -> ReaderId -> ReaderState
lookupReader (ChainProducerState _ rs) rid = r
  where
    Just r = find (\r -> readerId r == rid) rs

readerInstruction :: ChainProducerState rep
                  -> ReaderId
                  -> Maybe (ChainProducerState rep, ConsumeChain Block)
readerInstruction cps rid =
    Nothing
  where
    _r = lookupReader cps rid

{--
  - applyChainProducerUpdate :: ChainUpdate -> ChainProducerState -> ChainProducerState
  - applyChainProducerUpdate cu (cps@ChainProducerState {chainState})
  -     = (normalizeChainProducerState cu cps) { chainState = applyChainStateUpdate cu chainState }
  -
  - invApplyChainProducerUpdate :: ChainUpdate -> ChainProducerState ->  Bool
  - invApplyChainProducerUpdate cu cps = case applyChainProducerUpdate cu cps of
  -     ChainProducerState
  -         { chainState    = ChainState (Volatile blocks _)
  -         , chainReaders
  -         } -> and
  -             [
  -               -- all pointers should be still in volatile chain
  -               and [ Map.member intersectionBlockId blocks && Map.member readerBlockId blocks
  -                   | ReaderState
  -                       { readerIntersection = (_, intersectionBlockId)
  -                       , readerHead         = (_, readerBlockId)
  -                       } <- chainReaders
  -                   ]
  -             ]
  --}

data ConsumeChain block = RollForward  block
                        | RollBackward Point


--
-- Final simulation property
--

--TODO !
--
-- The general simulation propert for a suitablely constrained sequence of
-- the concrete operations.
--
-- The flush and prune operations allow quite a bit of flexibility about when
-- we do them, but there is a constraint that we flush before we prune so
-- that we do not break the chain overlap.
--
-- Could pick a specific flush policy but would like to check that an arbitrary
-- valid policy is still ok.
