{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Base part of /follow-the-satoshi/ procedure.

module Pos.Lrc.Fts
       ( followTheSatoshiM
       ) where

import           Control.Lens       (makeLenses, makePrisms, uses)
import           Data.List.NonEmpty (fromList)
import           Universum

import           Pos.Core.Coin      (coinToInteger, unsafeGetCoin)
import           Pos.Core.Constants (epochSlots)
import           Pos.Core.Types     (Coin, LocalSlotIndex (..), SharedSeed (..),
                                     SlotLeaders, StakeholderId, mkCoin)

import           Pos.Crypto         (deterministic, randomNumber)
import           Pos.Util.Iterator  (MonadIterator (..))

-- | Whereas 'Coin' stores an amount of coins, 'CoinIndex' is an identifier
-- for a particular coin.
--
-- Here's an example where we assign indices to particular coins that belong
-- to particular stakeholders:
--
-- StakeholderId | Coin (amount) | Coin indices (inclusive range)
-- --------------+---------------+-----------------------------
--           xxx | 200           |   [0..199]
--           yyy | 50            | [200..249]
--           zzz | 80            | [250..329]
--
-- In other words, 'Coin' is a cardinal, whereas 'CoinIndex' is an ordinal.
newtype CoinIndex = CoinIndex Word64
    deriving (Show, Eq, Ord, Num, Enum)

makePrisms ''CoinIndex

-- | Offset a coin index by a given amount of coins.
coinIndexOffset :: Coin -> CoinIndex -> CoinIndex
coinIndexOffset c = over _CoinIndex (+ unsafeGetCoin c)

-- | Assign a local slot index to each value in a list, starting with
-- @LocalSlotIndex 0@.
assignToSlots :: [a] -> [(LocalSlotIndex, a)]
assignToSlots = zip [minBound..]

-- | Sort values by their local slot indices, then strip the indices.
arrangeBySlots :: [(LocalSlotIndex, a)] -> [a]
arrangeBySlots = map snd . sortWith fst

-- | The internal state for the 'followTheSatoshiM' algorithm.
data FtsState = FtsState
    { _fsCurrentStakeholder         :: !StakeholderId
    , _fsCurrentCoinRangeUpperBound :: !CoinIndex
    }

makeLenses ''FtsState

-- | Initialize the internal state for the 'followTheSatoshiM' algorithm by
-- the first stakeholder.
ftsStateInit :: (StakeholderId, Coin) -> FtsState
ftsStateInit (adr, val) = FtsState adr (coinIndexOffset val 0)

-- | Update the internal state for the 'followTheSatoshiM' algorithm
-- with the next stakeholder.
ftsStateUpdate :: (StakeholderId, Coin) -> FtsState -> FtsState
ftsStateUpdate (adr, val) =
    set fsCurrentStakeholder adr .
    over fsCurrentCoinRangeUpperBound (coinIndexOffset val)

-- | Retrieve the next stakeholder.
nextStakeholder
    :: MonadIterator (StakeholderId, Coin) m
    => m (StakeholderId, Coin)
nextStakeholder =
    fromMaybe (error "followTheSatoshiM: indices out of range") <$> nextItem

{- |

Select slot leaders for an entire epoch, randomly. The probability that a
stakeholder will be chosen as a slot leader is directly proportional to his
stake. The same stakeholder can be chosen for multiple slots within an epoch.

As input, the /follow-the-satoshi/ procedure requires:

* a source of randomness (PRNG seed).
* the list of balances, i.e. (StakeholderId, Coin) pairs, ordered by
  stakeholder identifiers. Since the list can be quite large, we access it
  using 'MonadIterator' to achieve streaming.
* the total amount of coins in the system. We could compute it as the sum of
  balances (indeed, it is assumed to be equal to that value), but computing it
  here would break streaming, so we take it as a separate input.

As an output, we have one slot leader per slot: a list of stakeholder
identifiers of length 'epochSlots'.

The algorithm here is a bit tricky to ensure that it runs in O(balances) time.
First we generate a random coin index for each slot, so we get a list like
this:

@
  'assignToSlots' coinIndices =
    [ (LocalSlotIndex 0, CoinIndex 5)
    , (LocalSlotIndex 1, CoinIndex 327)
    , (LocalSlotIndex 2, CoinIndex 61)
    , ...
    , (LocalSlotIndex k, CoinIndex 205) ]

    where k = epochSlots - 1
@

The coin indices are random numbers in range @[0..totalCoins-1]@.

Then we want to find the owners of the coins with the generated indices.
In the example above, the stakeholder that owns the 5th coin will be the leader
of the 0th slot in the epoch, the owner of the 327th coin will be the leader
of the 1st slot, and so on.

Recall that we have a sorted list of balances. We could use it to compute a
table of coin indices, for example:

@
  StakeholderId | Coin (amount) | Coin indices (inclusive range)
  --------------+---------------+-----------------------------
            aaa | 200           |   [0..199]
            bbb | 50            | [200..249]
            ccc | 80            | [250..329]
            ... | ...           | ...
            zzz | 500           | [totalCoins - 1 - 500 .. totalCoins - 1]
@

A solution with complexity O(slots * balances) would be straightforward: just
lookup a leader for each slot in the table - whenever the selected coin is in
the coin index range, we have a match.

However, we can be a bit smarter and get an asymptotic improvement. First,
let's sort the generated coin indices:

@
    [ (LocalSlotIndex 0, CoinIndex 5)
    , (LocalSlotIndex 2, CoinIndex 61)
    , (LocalSlotIndex k, CoinIndex 205)
    , (LocalSlotIndex 1, CoinIndex 327)
    , ... ]
@

Since they are zipped together with their slot indices, we can restore the
original ordering later (using 'arrangeBySlots').

Now that selected coin indices and stakeholder coin index ranges are both
sorted, we can walk through them in one pass! To do that, we 'traverse' the
list of selected coin indices. As we do that, we monadically iterate through
the table of balances, storing current stakeholder identifier and coin index
range in a state (see 'FtsState').

Let's take a look at the intermediate states during execution:

1.  Initialize the state as @StakeholderId=aaa, CoinRange=[0..199]@.

2.  Select the next coin, @LocalSlotIndex=0, CoinIndex=5@.

3.  @CoinIndex@ falls within the @CoinRange@, yield slot leader
    @LocalSlotIndex=0, StakeholderId=aaa@.

5.  Select the next coin, @LocalSlotIndex=2, CoinIndex=61@.

6.  @CoinIndex@ falls within the @CoinRange@, yield slot leader
    @LocalSlotIndex=2, StakeholderId=aaa@.

7.  Select the next coin, @LocalSlotIndex=k, CoinIndex=205@.

8.  @CoinIndex@ does not fall within the @CoinRange@.
    Update the state to @StakeholderId=bbb, CoinRange=[200..249]@.

9.  @CoinIndex@ falls within the @CoinRange@, yield slot leader
    @LocalSlotIndex=k, StakeholderId=bbb@.

10. Select the next coin, @LocalSlotIndex=1, CoinIndex=327@.

11. @CoinIndex@ does not fall within the @CoinRange@.
    Update the state to...

Eventually, we end up with a list of slot leaders:

@
    [ (LocalSlotIndex 0, StakeholderId aaa)
    , (LocalSlotIndex 2, StakeholderId aaa)
    , (LocalSlotIndex k, StakeholderId bbb)
    , (LocalSlotIndex 1, StakeholderId ccc)
    , ... ]
@

and we're done. One thing to notice is that 'MonadIterator' provides us with a
balance - the amount of coins a stakeholder has - but not with a coin index
range. So in order to have access to coin range, we have to accumulate (by
summation) those balances - this way we get upper bounds. There's no need to
calculate lower bounds, since every lower bound is equal to the upper bound of
the previous stakeholder plus 1, and we transition to next coin only when the
current coin index exceeds the upper bound.  Since coin indices are sorted,
whenever we transition to the next coin, its index bona fide exceeds the
previous upper bound (and thus it's more or equal to the current lower bound).

-}
followTheSatoshiM
    :: forall m . MonadIterator (StakeholderId, Coin) m
    => SharedSeed
    -> Coin
    -> m SlotLeaders
followTheSatoshiM _ totalCoins
    | totalCoins == mkCoin 0 = error "followTheSatoshiM: nobody has any stake"
followTheSatoshiM (SharedSeed seed) totalCoins = do
    ftsState <- ftsStateInit <$> nextStakeholder
    let sortedCoinIndices = sortWith snd (assignToSlots coinIndices)
    res <- evaluatingStateT ftsState $ findLeaders sortedCoinIndices
    pure . fromList . arrangeBySlots $ res
  where
    coinIndices :: [CoinIndex]
    coinIndices = map (CoinIndex . fromInteger) . deterministic seed $
        replicateM epochSlots (randomNumber (coinToInteger totalCoins))

    findLeaders = (traverse . _2) findLeader

    findLeader :: CoinIndex -> StateT FtsState m StakeholderId
    findLeader coinIndex = do
        inRange <- uses fsCurrentCoinRangeUpperBound (coinIndex <=)
        if inRange
            then do
                -- Coin index falls within the range of coins that belong to
                -- the current stakeholder. We yield the stakeholder and move
                -- on to the next coin.  Notice that we don't move to the next
                -- stakeholder! The next coin can belong to the same
                -- stakeholder.
                use fsCurrentStakeholder
            else do
                -- Coin index does not fall within the range of coins
                -- that belong to the current stakeholder. Since coin indices
                -- are sorted, it is safe to assume that upcoming coins won't
                -- belong to this stakeholder either. We move on to the next
                -- stakeholder and retry the current coin.
                s <- nextStakeholder
                modify (ftsStateUpdate s)
                findLeader coinIndex
