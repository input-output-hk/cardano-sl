
-- | Base part of /follow-the-satoshi/ procedure.

module Pos.Lrc.Fts
       ( followTheSatoshi
       , followTheSatoshiM
       ) where

import           Universum

import           Control.Lens (makeLenses, makePrisms, uses)
import           Data.Conduit (ConduitT, (.|), runConduitPure, await)
import qualified Data.Conduit.List as CL
import           Data.List.NonEmpty (fromList)

import           Formatting (int, sformat, (%))

import           Pos.Core.Common (Coin, SharedSeed (..), SlotLeaders, StakeholderId, coinToInteger,
                                  mkCoin, sumCoins, unsafeGetCoin)
import           Pos.Core.Configuration (epochSlots, HasProtocolConstants)
import           Pos.Core.Slotting (LocalSlotIndex (..))
import           Pos.Crypto (deterministic, randomNumber)

-- Note: The "Satoshi" is the smallest indivisble unit of a Bitcoin.
-- The currency of the Cardano network is called "Ada" and its smallest
-- indivisible unit is called the "Lovelace".

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
assignToSlots :: HasProtocolConstants => [a] -> [(LocalSlotIndex, a)]
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

{- |

Select slot leaders for an entire epoch, randomly. The probability that a
stakeholder will be chosen as a slot leader is directly proportional to his
stake. The same stakeholder can be chosen for multiple slots within an epoch.

As input, the /follow-the-satoshi/ procedure requires:

* a source of randomness (PRNG seed).
* the list of stakes, i.e. (StakeholderId, Coin) pairs, ordered by
  stakeholder identifiers. Since the list can be quite large, we access it
  using 'conduit' to achieve streaming.
* the total amount of coins in the system. We could compute it as the sum of
  stakes (indeed, it is assumed to be equal to that value), but computing it
  here would break streaming, so we take it as a separate input.

As an output, we have one slot leader per slot: a list of stakeholder
identifiers of length 'epochSlots'.

The algorithm here is a bit tricky to ensure that it runs in O(stakes) time.
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

Recall that we have a sorted list of stakes. We could use it to compute a
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

A solution with complexity O(slots * stakes) would be straightforward: just
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
the table of stakes, storing current stakeholder identifier and coin index
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
stakes - the amount of coins a stakeholder has - but not with a coin index
range. So in order to have access to coin range, we have to accumulate (by
summation) those stakes - this way we get upper bounds. There's no need to
calculate lower bounds, since every lower bound is equal to the upper bound of
the previous stakeholder plus 1, and we transition to next coin only when the
current coin index exceeds the upper bound.  Since coin indices are sorted,
whenever we transition to the next coin, its index bona fide exceeds the
previous upper bound (and thus it's more or equal to the current lower bound).

-}

-- | Choose several random stakeholders (specifically, their amount is
-- currently hardcoded in 'Pos.Constants.epochSlots'). It is important to
-- note that the only source of random-ness in this compuation is the shared
-- random seed passed in as the first parameter.
--
-- The probability that a stakeholder will be chosen is proportional to the
-- number of coins this stakeholder holds. The same stakeholder can be picked
-- more than once.
--
-- How the algorithm works: we sort all unspent outputs in a deterministic
-- way (lexicographically) and have an ordered sequence of pairs
-- @(StakeholderId, Coin)@. Then we choose several random 'i's between 1 and
-- amount of Lovelace (the smallest indivisible unit of the Ada currency) in
-- the system; to find owner of 'i'th coin we find the lowest x such that sum
-- of all coins in this list up to 'i'th is not less than 'i' (and then 'x'th
-- address is the owner).
--
-- With P2SH addresses, we don't know who is going to end up with funds sent
-- to them. Therefore, P2SH addresses can contain 'addrDestination' which
-- specifies which addresses should count as “owning” funds for the purposes
-- of follow-the-satoshi.
followTheSatoshiM
    :: forall m . (Monad m, HasProtocolConstants)
    => SharedSeed
    -> Coin
    -> ConduitT (StakeholderId, Coin) Void m SlotLeaders
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
        replicateM (fromIntegral epochSlots)
            (randomNumber (coinToInteger totalCoins))

    findLeaders = (traverse . _2) findLeader

    findLeader :: CoinIndex ->
                  StateT FtsState (ConduitT (StakeholderId, Coin) Void m) StakeholderId
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
                s <- lift nextStakeholder
                modify (ftsStateUpdate s)
                findLeader coinIndex

    nextStakeholder = fromMaybe (error "followTheSatoshiM: indices out of range") <$> await


-- | A pure version of `followTheSatoshiM` above.
-- This pure version is used for testing and benchmarking. Its important to
-- note that since the ordering of the input stakes influences ths output,
-- testing this pure version as a proxy for the one above is insufficient.
-- The monadic version needs to be tested in conjunction with the same conduit
-- source that will feed it values in the real system.
followTheSatoshi :: (HasProtocolConstants) => SharedSeed -> [(StakeholderId, Coin)] -> SlotLeaders
followTheSatoshi seed stakes
    | totalCoins > coinToInteger maxBound =
        error $ sformat
        ("followTheSatoshi: total stake exceeds limit ("%int%" > "%int%")")
        totalCoins (coinToInteger maxBound)
    | totalCoinsCoin == minBound = error "followTheSatoshi: no stake"
    | otherwise =
          runConduitPure $ CL.sourceList stakes .|
                           followTheSatoshiM seed totalCoinsCoin
  where
    totalCoins = sumCoins $ map snd stakes
    totalCoinsCoin = mkCoin $ fromInteger totalCoins
