module Cardano.Wallet.Kernel.Submission (
    -- * Public API
      newWalletSubmission
    , addPending
    , remPending
    , tick

    -- * Types and lenses
    , Evicted
    , ResubmissionFunction
    , Schedule (..)
    , Scheduled (..)
    , Scheduler (..)
    , Slot
    , WalletSubmission
    , wsResubmissionFunction
    , getCurrentSlot
    , localPendingSet
    , getScheduler

    -- * Resubmitting things to the network
    , constantBroadcastIO
    , maxRetries
    , retryWith

    -- * Testing utilities
    , addOneToPending
    , unsafeSchedule
    , genWalletSubmission
    ) where

import           Universum

import qualified Control.Concurrent.Async as Async
import           Control.Lens (Getter, to)
import           Control.Lens.TH
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as M
import           Data.Text.Buildable (build)
import           Formatting (bprint, (%))
import qualified Formatting as F
import           Serokell.Util.Text (listJsonIndent, mapBuilder, pairF)
import           Test.QuickCheck

import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (Pending (..), emptyPending, genPending,
                                                pendingTransactions)
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

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
    , _wssScheduler   ::  Scheduler
    , _wssCurrentSlot :: !Slot
    }

instance Buildable WalletSubmissionState where
    build wss = bprint ("{ pendingSet = " % F.build %
                        ", scheduler  = " % F.build %
                        ", slot       = " % F.build %
                        " } "
                       ) (_wssPendingSet wss) (_wssScheduler wss) (_wssCurrentSlot wss)

-- | A 'Scheduler'. Despite modelled as in 'IntMap' it has to be intended
-- as a mapping between 'Slot' and the list of transactions due that slot.
data Scheduler = Scheduler {
    _ssScheduled :: IntMap [Scheduled]
    }

newtype Scheduled = Scheduled (Core.TxId, Core.TxAux, Schedule)

instance Buildable Scheduled where
    build (Scheduled (txId, _, s)) = bprint pairF (txId, s)

instance Buildable [Scheduled] where
    build s = bprint (listJsonIndent 4) s

-- | Our \"opaque\" concept of 'Slot', which might or might not line up with
-- the 'Core.FlatSlotId' of the blockchain.
-- Modelled as an 'Int' to tap into the performance of things like 'IntMap's,
-- and enough to keep a ticker running for hundreds of years. Remember this is
-- not the lifetime of the blockchain: it has more to do with the lifetime of
-- the wallet, as it will reset to 0 each time we restart it (the entire
-- 'WalletSubmission' is ephimeral and not persisted on disk).
type Slot = Int

-- | How many times we have tried to submit the given transaction.
-- When this value reaches the 'maxRetries' value, the transcation will be
-- removed from the local pending set.
-- Note that when the @Core@ layer will introduce the concept of \"Time to
-- Live\" for transactions, we will be able to remove the 'maxRetries' value
-- and simply use the @TTL@ to judge whether or not we should retry.
type SubmissionCount = Int

-- | A default and very generous time to live.
maxRetries :: SubmissionCount
maxRetries = 255

-- | The 'Schedule' for a 'Core.TxAux'.
newtype Schedule = Schedule SubmissionCount

instance Buildable Schedule where
    build (Schedule s) = bprint F.build s


-- | The 'Evicted' set represents the transactions which needs to be
-- pruned from the local (and wallet) 'Pending' set.
type Evicted = Pending

-- | A 'ResubmissionFunction' (@rho@ in the spec), parametrised by an
-- arbitrary @m@.
type ResubmissionFunction m =  Slot
                            -- ^ The current slot
                            -> [Scheduled]
                            -- ^ Transactions which are due
                            -> Scheduler
                            -- ^ The original 'WalletSubmissionState'
                            -> m (Evicted, Scheduler)
                            -- ^ The transactions to remove together with
                            -- the new 'Scheduler'.

makeLenses ''Scheduler
makeLenses ''WalletSubmission
makeLenses ''WalletSubmissionState

instance Buildable Scheduler where
    build ss =
        let elems = ss ^. ssScheduled . to IntMap.toList
        in bprint (F.later mapBuilder) elems


instance Arbitrary Schedule where
    arbitrary = Schedule <$> choose (0, maxRetries)

-- Generates a random schedule by picking a slot >= of the input one but
-- within a 'slot + 10' range, as really generating schedulers which generates
-- things too far away in the future is not very useful for testing, if not
-- testing that a scheduler will never reschedule something which cannot be
-- reached.
genScheduler :: Pending -> Slot -> Gen Scheduler
genScheduler pending lowerBound = do
    let pendingTxs  = pending ^. pendingTransactions . fromDb . to M.toList
    slots <- vectorOf (length pendingTxs) (choose (lowerBound, lowerBound + 10))
    retries    <- vectorOf (length pendingTxs) (choose (0, maxRetries))
    return $ Scheduler $ List.foldl' updateFn mempty (zip3 slots pendingTxs retries)
    where
        updateFn acc (slot, (txId, txAux), retries) =
            let s = Scheduled (txId, txAux, Schedule retries)
            in IntMap.insertWith mappend slot [s] acc

instance Arbitrary WalletSubmissionState where
    arbitrary = do
        pending   <- genPending (Core.ProtocolMagic 0)
        slot      <- fmap getPositive arbitrary
        scheduler <- genScheduler pending slot
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
            , _wssCurrentSlot = 0
            , _wssScheduler   = Scheduler IntMap.empty
            }

-- | Informs the 'WalletSubmission' layer about new 'Pending' transactions.
addPending :: WalletSubmission m -> Pending -> WalletSubmission m
addPending ws newPending =
    let ws' = ws & over (wsState . wssPendingSet) (unionPending newPending)
    in schedule ws' newPending

-- | Removes the input 'Pending' from the local 'WalletSubmission' pending set.
remPending :: WalletSubmission m -> Pending -> WalletSubmission m
remPending ws updatedPending =
    ws & over (wsState . wssPendingSet) (flip differencePending updatedPending)

-- | A \"tick\" of the scheduler.
-- Returns the set transactions which needs to be droppped by the system as
-- they likely exceeded the submission count and they have no chance to be
-- adopted in a block, or that have simply been successfully retrasmitted.
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
        scheduler         = ws  ^. getScheduler
        rho               = _wsResubmissionFunction ws
        dueNow            = dueThisSlot currentSlot wss
    (toPrune, scheduler') <- rho currentSlot dueNow scheduler
    let newState = ws & over (wsState . wssScheduler) (const (purgeSlot currentSlot scheduler'))
                      . over (wsState . wssCurrentSlot) succ
    return (toPrune , newState)

-- | Overrides the 'Scheduler' with an input transaction and a
-- custom 'Schedule'. Useful to force dispatching, especially in tests.
unsafeSchedule :: WalletSubmission m
               -> Slot
               -> Scheduled
               -> WalletSubmission m
unsafeSchedule ws slot scheduled@(Scheduled (txId, txAux, _)) =
    ws & over (wsState . wssPendingSet) (addOneToPending txId txAux)
       . over (wsState . wssScheduler) overrideSchedule
    where
        overrideSchedule :: Scheduler -> Scheduler
        overrideSchedule (Scheduler s) =
            Scheduler (IntMap.insertWith mappend slot [scheduled] s)


-- | A getter to the local pending set stored in this 'WalletSubmission'.
localPendingSet :: Getter (WalletSubmission m) Pending
localPendingSet = wsState . wssPendingSet

getCurrentSlot :: Getter (WalletSubmission m) Slot
getCurrentSlot = wsState . wssCurrentSlot

getScheduler :: Getter (WalletSubmission m) Scheduler
getScheduler = wsState . wssScheduler


--
-- Internal API
--

-- | Schedule the full list of pending transactions.
-- The transactions will be scheduled immediately in the next 'Slot'.
schedule :: WalletSubmission m
         -> Pending
         -> WalletSubmission m
schedule ws pending =
    let currentSlot = ws ^. wsState . wssCurrentSlot
    in ws & over (wsState . wssScheduler) (scheduleNext currentSlot)
    where
        scheduleNext :: Slot -> Scheduler -> Scheduler
        scheduleNext currentSlot (Scheduler s) =
            let txs     = map toEntry (pending ^. pendingTransactions . fromDb . to M.toList)
                toEntry (txId, txAux) = Scheduled (txId, txAux, Schedule 0)
            in Scheduler (IntMap.insertWith mappend (succ currentSlot) txs s)

-- | Purges all the scheduled transactions in the given 'Slot'. This is
-- automatically called by 'tick', and it's responsibility of @rho@ to
-- correctly reschedule all the transaction.
purgeSlot :: Slot -> Scheduler -> Scheduler
purgeSlot slot (Scheduler s) = Scheduler (IntMap.delete slot s)

-- | Returns a set of 'Pending' transactions which are due in the given
-- 'Slot'.
-- TODO(adn): deal with transaction sorting etc.
dueThisSlot :: Slot
            -- ^ The current 'Slot'.
            -> WalletSubmissionState
            -- ^ The 'WalletSubmissionState'.
            -> [Scheduled]
            -- ^ The filtered set of 'Pending' which are due in the input slot.
dueThisSlot currentSlot wss =
    let scheduler  = wss ^. wssScheduler . to _ssScheduled
    in fromMaybe mempty (IntMap.lookup currentSlot scheduler)


-- TODO(adn): Better placed into module 'Cardano.Wallet.Kernel.DB.Spec'?

-- | Computes the union between two 'Pending' sets.
unionPending :: Pending -> Pending -> Pending
unionPending (Pending new) (Pending old) =
    Pending (M.union <$> new <*> old)

-- | Computes the difference between two 'Pending' sets.
differencePending :: Pending -> Pending -> Pending
differencePending (Pending new) (Pending old) =
    Pending (M.difference <$> new <*> old)

-- | Adds a single element to the 'Pending' set.
addOneToPending :: Core.TxId -> Core.TxAux -> Pending -> Pending
addOneToPending txId aux (Pending p) = Pending (M.insert txId aux <$> p)

-- Ready-to-use 'ResubmissionFunction's.

-- | A very lenient resubmitter which blindly schedules each pending transaction
-- to be resubmitted the next slot.
-- NOTE(adn): In this function we are submitting all the due transactions first,
-- and only later check if we exceeded the submission count. This is slightly more taxing
-- on the network layer but not a real problem: in the worst case scenario we
-- would simply send a transaction to the neighbours twice.
constantBroadcastIO :: WalletDiffusion -> ResubmissionFunction IO
constantBroadcastIO diffusion currentSlot scheduled oldScheduler = do
    results <- broadcast scheduled
    let (evicted, newScheduler) = List.foldl' updateFn (emptyPending, oldScheduler) results
    return ( evicted , newScheduler)
    where

        broadcast :: [Scheduled] -> IO [(Scheduled, Either SomeException Bool)]
        broadcast = Async.mapConcurrently trySubmit

        trySubmit :: Scheduled
                  -> IO (Scheduled, Either SomeException Bool)
        trySubmit s@(Scheduled (_, aux, _)) = do
            newJob <- Async.async (walletSendTx diffusion aux)
            (s,) <$> Async.waitCatch newJob

        updateFn :: (Evicted, Scheduler)
                 -> (Scheduled, Either SomeException Bool)
                 -> (Evicted, Scheduler)
        updateFn (evicted, acc@(Scheduler s)) (entry, result) =
            let Scheduled (txId, txAux, (Schedule retries)) = entry
                rescheduled = retryWith succ entry
            in case retries >= maxRetries of
                   True -> (addOneToPending txId txAux evicted, acc)
                   False -> case result of
                       Left _      ->
                         let s' = IntMap.insertWith mappend
                                                    (currentSlot + 2)
                                                    [rescheduled]
                                                    s
                         in (evicted, Scheduler s')
                       Right False ->
                         let s' = IntMap.insertWith mappend
                                                    (currentSlot + 1)
                                                    [rescheduled]
                                                    s
                         in (evicted, Scheduler s')
                       Right True  ->
                         (addOneToPending txId txAux evicted, acc)


-- | Modifies the 'SubmissionCount' of the given 'Scheduled' entry by the
-- supplied function.
retryWith :: (SubmissionCount -> SubmissionCount) -> Scheduled -> Scheduled
retryWith fn (Scheduled (t, a, Schedule retries)) = Scheduled (t, a, Schedule (fn retries))
