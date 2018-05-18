{-# LANGUAGE BangPatterns #-}
module Cardano.Wallet.Kernel.Submission (
    -- * Public API
      newWalletSubmission
    , addPending
    , remPending
    , tick
    , dueThisSlot

    -- * Types and lenses
    , Evicted
    , ResubmissionFunction
    , SubmissionCount (..)
    , ScheduledTx (..)
    , Schedule (..)
    , Slot (..)
    , WalletSubmission
    , mapSlot
    , wsResubmissionFunction
    , getCurrentSlot
    , localPendingSet
    , getSchedule
    , overrideSchedule

    -- * Resubmitting things to the network
    , defaultResubmitFunction

    -- * Retry policies
    , constantRetry
    , exponentialBackoff

    -- * Testing utilities
    , genWalletSubmission
    ) where

import           Universum

import           Control.Lens (Getter, to)
import           Control.Lens.TH
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as M
import           Data.Text.Buildable (build)
import           Formatting (bprint, (%))
import qualified Formatting as F
import           Pos.Crypto.Hashing (WithHash (..))
import           Pos.Txp.Topsort (topsortTxs)
import           Serokell.Util.Text (listJsonIndent, mapBuilder, pairF)
import           Test.QuickCheck

import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (Pending (..), differencePending, emptyPending,
                                                genPending, pendingTransactions, singletonPending,
                                                unionPending)
import qualified Pos.Core as Core

-- | Wallet Submission Layer
--
-- This module implements section 9 of the Wallet spec,
-- namely 'Transaction Submission'.
--
data WalletSubmission m = WalletSubmission {
      _wsResubmissionFunction :: ResubmissionFunction m
    , _wsState                :: WalletSubmissionState
    }

instance Buildable (WalletSubmission m) where
    build ws = bprint ("WalletSubmission <rho> " % F.build) (_wsState ws)

data WalletSubmissionState = WalletSubmissionState {
      _wssPendingSet  ::  Pending
    , _wssSchedule    ::  Schedule
    , _wssCurrentSlot :: !Slot
    }

instance Buildable WalletSubmissionState where
    build wss = bprint ("{ pendingSet = " % F.build %
                        ", scheduler  = " % F.build %
                        ", slot       = " % F.build %
                        " } "
                       ) (_wssPendingSet wss) (_wssSchedule wss) (getSlot $ _wssCurrentSlot wss)

-- | A 'Schedule'. Despite modelled as in 'IntMap' it has to be intended
-- as a mapping between 'Slot' and the list of transactions due that slot.
newtype Schedule = Schedule {
    _ssScheduled :: IntMap [ScheduledTx]
    }

data ScheduledTx = ScheduledTx {
      _scheduledTxId            :: Core.TxId
      -- ^ The 'Core.TxId' corresponding to the scheduled 'Core.TxAux'. Stored
      -- together with the @TxAux@ itself to avoid a potentially expensive
      -- hash calculation to compute such @TxId@.
    , _scheduledTxAux           :: Core.TxAux
      -- ^ The scheduled transaction.
    , _scheduledSubmissionCount :: SubmissionCount
      -- ^ How many times we tried resubmitting this transaction.
    } deriving Eq

instance Buildable ScheduledTx where
    build (ScheduledTx txId _ s) = bprint pairF (txId, s)

instance Buildable [ScheduledTx] where
    build s = bprint (listJsonIndent 4) s

-- | Our \"opaque\" concept of 'Slot', which might or might not line up with
-- the 'Core.FlatSlotId' of the blockchain.
-- Modelled as an 'Word', but we cast it to an 'Int' to tap into the performance
-- of things like 'IntMap's, and enough to keep a ticker running for a good while.
-- Remember this is not the lifetime of the blockchain: it has more to do with
-- the lifetime of the wallet, as it will reset to 0 each time we restart it (the entire
-- 'WalletSubmission' is ephimeral and not persisted on disk).
--
-- The acute reader might ask why we are casting to 'Int' and what is the
-- implication of a possible overflow: in practice, none, as in case we overflow
-- the 'Int' positive capacity we will effectively treat this as a \"circular buffer\",
-- storing the elements for slots @(maxBound :: Int) + 1@ in negative positions.
newtype Slot = Slot { getSlot :: Word } deriving (Eq, Show)

castSlot :: Slot -> Int
castSlot (Slot w) = fromIntegral w

mapSlot :: (Word -> Word) -> Slot -> Slot
mapSlot f (Slot w) = Slot (f w)

-- | How many times we have tried to submit the given transaction.
-- When this value reaches the 'maxRetries' value, the transcation will be
-- removed from the local pending set.
-- Note that when the @Core@ layer will introduce the concept of \"Time to
-- Live\" for transactions, we will be able to remove the 'maxRetries' value
-- and simply use the @TTL@ to judge whether or not we should retry.
newtype SubmissionCount = SubmissionCount { getSubmissionCount :: Int } deriving Eq

instance Buildable SubmissionCount where
    build (SubmissionCount s) = bprint F.build s

-- | The 'Evicted' set represents the transactions which needs to be
-- pruned from the local (and wallet) 'Pending' set.
type Evicted = Pending

-- | A 'ResubmissionFunction' (@rho@ in the spec), parametrised by an
-- arbitrary @m@.
type ResubmissionFunction m =  Slot
                            -- ^ The current slot. Handy to pass to this
                            -- function to reschedule transactions to some
                            -- other 'Slot' + N.
                            -> [ScheduledTx]
                            -- ^ Transactions which are due
                            -> Schedule
                            -- ^ The original 'WalletSubmissionState'
                            -> m (Evicted, Schedule)
                            -- ^ The transactions to remove together with
                            -- the new 'Schedule'.

makeLenses ''Schedule
makeLenses ''WalletSubmission
makeLenses ''WalletSubmissionState

instance Buildable Schedule where
    build ss =
        let elems = ss ^. ssScheduled . to IntMap.toList
        in bprint (F.later mapBuilder) elems


instance Arbitrary SubmissionCount where
    arbitrary = SubmissionCount <$> choose (0, 255)

-- Generates a random schedule by picking a slot >= of the input one but
-- within a 'slot + 10' range, as really generating schedulers which generates
-- things too far away in the future is not very useful for testing, if not
-- testing that a scheduler will never reschedule something which cannot be
-- reached.
genSchedule :: Pending -> Slot -> Gen Schedule
genSchedule pending (Slot lowerBound) = do
    let pendingTxs  = pending ^. pendingTransactions . fromDb . to M.toList
    slots <- vectorOf (length pendingTxs) (fmap Slot (choose (lowerBound, lowerBound + 10)))
    retries    <- vectorOf (length pendingTxs) (choose (0, 255))
    return $ Schedule $ List.foldl' updateFn mempty (zip3 slots pendingTxs retries)
    where
        updateFn acc (slot, (txId, txAux), retries) =
            let s = ScheduledTx txId txAux (SubmissionCount retries)
            in IntMap.insertWith mappend (castSlot slot) [s] acc

instance Arbitrary WalletSubmissionState where
    arbitrary = do
        pending   <- genPending (Core.ProtocolMagic 0)
        slot      <- fmap Slot arbitrary
        scheduler <- genSchedule pending slot
        return $ WalletSubmissionState pending scheduler slot

genWalletSubmission :: ResubmissionFunction m -> Gen (WalletSubmission m)
genWalletSubmission rho =
    WalletSubmission <$> pure rho <*> arbitrary


--
-- Public API, as written in the spec
--

newWalletSubmission :: ResubmissionFunction m -> WalletSubmission m
newWalletSubmission resubmissionFunction = WalletSubmission {
      _wsResubmissionFunction = resubmissionFunction
    , _wsState = newEmptyState
    }
    where
        newEmptyState :: WalletSubmissionState
        newEmptyState = WalletSubmissionState {
              _wssPendingSet  = emptyPending
            , _wssCurrentSlot = Slot 0
            , _wssSchedule   = Schedule IntMap.empty
            }

-- | Informs the 'WalletSubmission' layer about new 'Pending' transactions.
addPending :: Pending -> WalletSubmission m -> WalletSubmission m
addPending newPending ws =
    let ws' = ws & over (wsState . wssPendingSet) (unionPending newPending)
    in schedulePending newPending ws'

-- | Removes the input 'Pending' from the local 'WalletSubmission' pending set.
remPending :: Pending -> WalletSubmission m -> WalletSubmission m
remPending updatedPending ws =
    ws & over (wsState . wssPendingSet) (flip differencePending updatedPending)

-- | A \"tick\" of the scheduler.
-- Returns the set transactions which needs to be droppped by the system as
-- they likely exceeded the submission count and they have no chance to be
-- adopted in a block.
-- @N.B.@ The returned 'WalletSubmission' does not come with an already-pruned
-- local 'Pending' set. It will be caller's responsibility to call 'remPending'
-- on the 'WalletSubmission'.
--
tick :: Monad m
     => WalletSubmission m
     -- ^ The current 'WalletSubmission'.
     -> m (Evicted, WalletSubmission m)
     -- ^ The set of transactions upper layers will need to drop, together
     -- with the new 'WalletSubmission'.
tick ws = do
    let wss               = ws  ^. wsState
        currentSlot       = wss ^. wssCurrentSlot
        schedule          = ws  ^. getSchedule
        rho               = _wsResubmissionFunction ws
        dueNow            = dueThisSlot currentSlot ws
    (toPrune, schedule') <- rho currentSlot dueNow schedule
    let newState = ws & over (wsState . wssSchedule) (const (purgeSlot currentSlot schedule'))
                      . over (wsState . wssCurrentSlot) (mapSlot succ)
    return (toPrune , newState)

-- | Returns a set of 'Pending' transactions which are due in the given
-- 'Slot'. First of all, it looks at the 'ScheduledTx' items (transactions) due
-- this slot, and sorts them topologically, and:
--
-- 1. If a topological order can be found, the 'ScheduledTx' items are returned.
-- 2. TODO(adn) If a topological order can't be found, we will need to deal with it.
--
dueThisSlot :: Slot
            -- ^ The current 'Slot'.
            -> WalletSubmission m
            -- ^ The 'WalletSubmissionState'.
            -> [ScheduledTx]
            -- ^ The filtered set of 'Pending' which are due in the input slot.
dueThisSlot currentSlot ws =
    let scheduler  = ws ^. getSchedule . to _ssScheduled
        slotted    = fromMaybe mempty (IntMap.lookup (castSlot currentSlot) scheduler)
        topSorted  = topsortTxs toTx slotted
    in case topSorted of
            Nothing     -> slotted
            Just sorted -> sorted
    where
        toTx :: ScheduledTx -> WithHash Core.Tx
        toTx (ScheduledTx txId txAux _) =  WithHash (Core.taTx txAux) txId

-- | Overrides the 'Schedule' with an input transaction and a
-- custom 'Schedule'. Useful to force dispatching, especially in tests.
overrideSchedule :: WalletSubmission m
                 -> Slot
                 -> ScheduledTx
                 -> WalletSubmission m
overrideSchedule ws slot scheduled = ws & over (wsState . wssSchedule) override
    where
        override :: Schedule -> Schedule
        override (Schedule s) =
            Schedule (IntMap.insertWith mappend (castSlot slot) [scheduled] s)


-- | A getter to the local pending set stored in this 'WalletSubmission'.
localPendingSet :: Getter (WalletSubmission m) Pending
localPendingSet = wsState . wssPendingSet

getCurrentSlot :: Getter (WalletSubmission m) Slot
getCurrentSlot = wsState . wssCurrentSlot

getSchedule :: Getter (WalletSubmission m) Schedule
getSchedule = wsState . wssSchedule


--
-- Internal API
--

-- | Schedule the full list of pending transactions.
-- The transactions will be scheduled immediately in the next 'Slot'.
schedulePending :: Pending
                -> WalletSubmission m
                -> WalletSubmission m
schedulePending pending ws =
    let currentSlot = ws ^. wsState . wssCurrentSlot
    in ws & over (wsState . wssSchedule) (scheduleNext currentSlot)
    where
        scheduleNext :: Slot -> Schedule -> Schedule
        scheduleNext currentSlot (Schedule s) =
            let txs     = map toEntry (pending ^. pendingTransactions . fromDb . to M.toList)
                toEntry (txId, txAux) = ScheduledTx txId txAux (SubmissionCount 0)
            in Schedule (IntMap.insertWith mappend (castSlot $ mapSlot succ currentSlot) txs s)

-- | Purges all the scheduled transactions in the given 'Slot'. This is
-- automatically called by 'tick', and it's responsibility of @rho@ to
-- correctly reschedule all the transaction.
purgeSlot :: Slot -> Schedule -> Schedule
purgeSlot slot (Schedule s) = Schedule (IntMap.delete (castSlot slot) s)

-- Ready-to-use 'ResubmissionFunction's.

-- | A 'RetryPolicy' is simply a function which instruct the 'Schedule' when
-- to attempt resubmitting the given 'ScheduledTx' item.
type RetryPolicy = SubmissionCount -> Slot -> Maybe Slot

limited :: MaxRetries -> RetryPolicy -> RetryPolicy
limited maxRetries retryPolicy = \retries currentSlot ->
    case getSubmissionCount retries >= maxRetries of
         True  -> Nothing
         False -> retryPolicy retries currentSlot

type Exponent   = Double
type MaxRetries = Int

--
-- Stock retry policies inspired by the 'retry' package.
--

-- | Very simple policy which merely retries to resubmit the very next 'Slot',
-- up until 'MaxRetries' attempts.
constantRetry :: MaxRetries -> RetryPolicy
constantRetry maxRetries =
    limited maxRetries $ \_ currentSlot -> Just (mapSlot succ currentSlot)

-- | An exponential backoff policy, parametric over a maximum number of
-- 'MaxRetries' and an 'Exponent' for the backoff.
exponentialBackoff :: MaxRetries -> Exponent -> RetryPolicy
exponentialBackoff maxRetries exponent =
    limited maxRetries $ \retries currentSlot ->
        Just $ mapSlot (\s -> s + floor (exponent ^^ (getSubmissionCount $ retries))) currentSlot

-- | A very customisable resubmitter which can be configured with different
-- retry policies.
defaultResubmitFunction :: forall m. Monad m
                        => (Core.TxAux -> m Bool)
                        -> RetryPolicy
                        -> ResubmissionFunction m
defaultResubmitFunction send retryPolicy currentSlot scheduled oldSchedule = do
    foldlM updateFn (emptyPending, oldSchedule) scheduled
    where
        updateFn :: (Evicted, Schedule) -> ScheduledTx -> m (Evicted, Schedule)
        updateFn (evicted, acc@(Schedule s)) entry = do
            let (ScheduledTx txId txAux retries) = entry
                reschedule targetSlot =
                    IntMap.insertWith mappend
                                      (castSlot targetSlot)
                                      [incSubmissionCount entry]
                                      s
            case retryPolicy retries currentSlot of
                 Nothing -> return (singletonPending txId txAux `unionPending` evicted, acc)
                 Just newSlot -> do
                     result <- send txAux
                     case result of
                         False -> return (evicted, Schedule (reschedule newSlot))
                         True  -> return (evicted, acc)

-- | Modifies the 'SubmissionCount' of the given 'ScheduledTx' entry by 1.
incSubmissionCount :: ScheduledTx -> ScheduledTx
incSubmissionCount (ScheduledTx t a (SubmissionCount retries)) =
    ScheduledTx t a (SubmissionCount $ retries + 1)
