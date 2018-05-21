{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ViewPatterns #-}
module Cardano.Wallet.Kernel.Submission (
    -- * Public API
      newWalletSubmission
    , addPending
    , remPending
    , tick
    , scheduledFor
    , tickSlot

    -- * Types and lenses
    , Evicted
    , ResubmissionFunction
    , Schedule
    , ScheduleEvents (..)
    , ScheduleSend (..)
    , NextEvent (..)
    , seToSend
    , seToConfirm
    , ScheduleConfirm (..)
    , SchedulingError (..)
    , Slot (..)
    , SubmissionCount (..)
    , WalletSubmission
    , mapSlot
    , wsResubmissionFunction
    , getCurrentSlot
    , localPendingSet
    , getSchedule
    , addToSchedule

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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text.Buildable (build)
import           Formatting (bprint, sformat, (%))
import qualified Formatting as F
import           Pos.Crypto.Hashing (WithHash (..))
import           Pos.Txp.Topsort (topsortTxs)
import qualified Prelude
import           Serokell.Util.Text (listJsonIndent, mapBuilder, pairF)
import           Test.QuickCheck

import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (Pending (..), differencePending, emptyPending,
                                                genPending, pendingTransactions, unionPending)
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

-- | A 'Schedule' of events.
data Schedule = Schedule {
      _ssScheduled     :: IntMap ScheduleEvents
    -- ^ Despite modelled as in 'IntMap' it has to be intended
    -- as a mapping between 'Slot' and the list of transactions due that slot.
    -- We do not store transactions directly but a richer type called 'ScheduleEvents',
    -- which partition the event space into items scheduled to be sent and
    -- items which needs to be checked for confirmation.
    , _ssUnsentNursery :: [ScheduleSend]
    -- ^ A list of unsent transactions which couldn't be sent due to dependency
    -- over transactions scheduled in some other slot. Practical example: Let
    -- @A@ be a transaction scheduled for slot @N + 3@ and let @B@ be a transaction
    -- @which depends on A@ scheduled for slot @N@. If we were to send @B@ we
    -- would make a mistake, as it cannot be adopted before @A@ does. The solution
    -- is to capture this event in 'tickSlot', and putting @B@ into the
    -- nursery up until it can be sent.
    -- @N.B@ It should be the wallet's responsibility (not the submission layer's)
    -- to make sure that when it gives up on a transaction @A@, it also gives
    -- up on all transactions @Bs@ that depend on @A@.
    }

-- | A type representing an item (in this context, a transaction) scheduled
-- to be regularly sent in a given slot (computed by a given 'RetryPolicy').
data ScheduleSend = ScheduleSend Core.TxId Core.TxAux SubmissionCount deriving Eq

-- | A type representing an item (in this context, a transaction @ID@) which
-- needs to be checked against the blockchain for inclusion. In other terms,
-- we need to confirm that indeed the transaction identified by the given 'TxId' has
-- been adopted, i.e. it's not in the local pending set anymore.
newtype ScheduleConfirm = ScheduleConfirm Core.TxId deriving Eq

-- | All the events we can schedule for a given 'Slot', partitioned into
-- 'ScheduleSend' and 'ScheduleConfirm'.
data ScheduleEvents = ScheduleEvents {
      _seToSend    :: [ScheduleSend]
    -- ^ A list of transactions which we need to send.
    , _seToConfirm :: [ScheduleConfirm]
    -- ^ A list of transactions which we need to check if they have been
    -- confirmed (i.e. adopted) by the blockchain.
    }

instance Buildable ScheduleSend where
    build (ScheduleSend   txId _ s) = bprint ("ScheduleSend " % pairF) (txId, s)

instance Buildable [ScheduleSend] where
    build s = bprint (listJsonIndent 4) s

instance Buildable ScheduleConfirm where
    build (ScheduleConfirm txId)     = bprint ("ScheduleConfirm " % F.build) txId

instance Buildable [ScheduleConfirm] where
    build s = bprint (listJsonIndent 4) s

instance Buildable ScheduleEvents where
    build (ScheduleEvents ss sc) =
        bprint ("ScheduleEvents { toCheck = " % F.build %
                               ", toConfirm = " % F.build %
                               "}") ss sc

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
newtype Slot = Slot { getSlot :: Word } deriving (Eq, Ord, Show)

instance Buildable Slot where
    build (Slot s) = bprint ("Slot " % F.build) s

castSlot :: Slot -> Int
castSlot (Slot w) = fromIntegral w

-- | Adds to 'Slot's together.
addSlot :: Slot -> Slot -> Slot
addSlot (Slot w1) (Slot w2) = Slot (w1 + w2)

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
type Evicted = Set Core.TxId

-- | A 'ResubmissionFunction' (@rho@ in the spec), parametrised by an
-- arbitrary @m@.
type ResubmissionFunction m =  Slot
                            -- ^ The current slot. Handy to pass to this
                            -- function to reschedule transactions to some
                            -- other 'Slot' + N.
                            -> [ScheduleSend]
                            -- ^ Transactions which are due this 'Slot'.
                            -> Pending
                            -> Schedule
                            -- ^ The original 'WalletSubmissionState'
                            -> m (Evicted, Schedule)
                            -- ^ The new 'Schedule'.

makeLenses ''ScheduleEvents
makeLensesFor [("_ssScheduled", "ssScheduled")] ''Schedule
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
    slots    <- vectorOf (length pendingTxs) (fmap Slot (choose (lowerBound, lowerBound + 10)))
    retries  <- vectorOf (length pendingTxs) (choose (0, 255))
    let events = List.foldl' updateFn mempty (zip3 slots pendingTxs retries)
    return $ Schedule events mempty
    where
        updateFn acc (slot, (txId, txAux), retries) =
            let s = ScheduleSend txId txAux (SubmissionCount retries)
                e = ScheduleEvents [s] mempty
            in IntMap.insertWith (\old _ -> old & over seToSend ((:) s)) (castSlot slot) e acc

instance Arbitrary WalletSubmissionState where
    arbitrary = do
        pending   <- genPending (Core.ProtocolMagic 0)
        slot      <- pure (Slot 0) -- Make the layer always start from 0, to make running the specs predictable.
        scheduler <- genSchedule pending slot
        return $ WalletSubmissionState pending scheduler slot

genWalletSubmission :: ResubmissionFunction m -> Gen (WalletSubmission m)
genWalletSubmission rho =
    WalletSubmission <$> pure rho <*> arbitrary

--
-- Public API, as written in the spec or mandated by real-world necessities.
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
            , _wssSchedule   = Schedule IntMap.empty mempty
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
     => (forall a. SchedulingError -> m a)
     -- ^ A callback to handle any potential error arising internally.
     -> WalletSubmission m
     -- ^ The current 'WalletSubmission'.
     -> m (Evicted, WalletSubmission m)
     -- ^ The set of transactions upper layers will need to drop, together
     -- with the new 'WalletSubmission'.
tick onError ws = do
    let wss         = ws  ^. wsState
        currentSlot = wss ^. wssCurrentSlot
        rho         = _wsResubmissionFunction ws
        pendingSet  = ws ^. wsState . wssPendingSet
    case tickSlot currentSlot ws of
         Left e -> onError e
         Right (toSend, toConfirm, newSchedule) -> do
            (evicted, schedule') <- rho currentSlot toSend pendingSet newSchedule
            let newState = ws & over (wsState . wssSchedule) (const schedule')
                              . over (wsState . wssCurrentSlot) (mapSlot succ)
            return (evicted, newState)

data SchedulingError =
    LoopDetected Pending
    -- ^ The transactions in this 'Pending' set forms a cycle and they
    -- couldn't be top-sorted.

instance Exception SchedulingError

-- | Instance required for 'Exception'. Giving this one a proper 'Show' instance
-- (via deriving instance or otherwise) would imply a Show instance for 'Pending'.
-- However, when dealing with data types which includes sensible data (like in
-- this case, transactions) it's usually better to sacrify ghci-readiness in
-- favour of a bit more anonymity.
instance Show SchedulingError where
    show (LoopDetected pending) = toString $ sformat ("LoopDetected " % F.build) pending

-- | Convenient \"list-destructuring-style\" data accessor which returns
-- the next events scheduled for the input 'Slot' as well as the \"tail\" of the
-- 'Schedule'.
-- It doesn't compute any sophisticated logic on the actual events which will
-- be eventually sent, nor tries to update the nursery. That is performed
-- specifically by the 'tickSlot' function.
scheduledFor :: Slot -> Schedule -> (ScheduleEvents, Schedule)
scheduledFor currentSlot s@(Schedule schedule nursery) =
    case IntMap.lookup (castSlot currentSlot) schedule of
         Nothing -> (ScheduleEvents mempty mempty, s)
         Just candidates ->
             (candidates, Schedule (IntMap.delete (castSlot currentSlot) schedule) nursery)

-- | Returns a set of 'Pending' transactions which are due in the given
-- 'Slot'.
-- TODO(adn) Document this.
--
tickSlot :: Slot
         -- ^ The current 'Slot'.
         -> WalletSubmission m
         -- ^ The 'WalletSubmissionState'.
         -> Either SchedulingError ([ScheduleSend], [ScheduleConfirm], Schedule)
         -- ^ An error if no schedule can be produced, or all the scheduled
         -- transactions together with the new, updated 'Schedule'.
tickSlot currentSlot ws =
    let (allEvents, schedule) = scheduledFor currentSlot (ws ^. wsState . wssSchedule)
        scheduledCandidates = allEvents ^. seToSend <> nursery schedule
        localPending = ws ^. wsState . wssPendingSet
        topSorted  = topsortTxs toTx scheduledCandidates
    in case topSorted of
            Nothing     -> Left (LoopDetected localPending)
            Just sorted ->
                let (cannotSend, send) = partitionNotSendable localPending sorted
                    newSchedule = schedule { _ssUnsentNursery = cannotSend }
                in Right (send, allEvents ^. seToConfirm, newSchedule)
    where
        nursery :: Schedule -> [ScheduleSend]
        nursery (Schedule _ n) = n

        toTx :: ScheduleSend -> WithHash Core.Tx
        toTx (ScheduleSend txId txAux _) =  WithHash (Core.taTx txAux) txId


partitionNotSendable :: Pending
                     -- ^ The local 'Pending' set.
                     -> [ScheduleSend]
                     -- ^ A @topologically sorted@ list of transactions scheduled
                     -- for being sent.
                     -> ([ScheduleSend], [ScheduleSend])
                     -- ^ A partition between transactions which cannot be sent
                     -- due to dependencies with future transactions and
                     -- transactions which are ready to be sent.
partitionNotSendable (view (pendingTransactions . fromDb) -> pending) xs =
    let allIds = Set.fromList $ map getTxId xs
    in go allIds (reverse xs) (mempty, mempty)
    where
        go :: Set Core.TxId
           -> [ScheduleSend]
           -> ([ScheduleSend], [ScheduleSend])
           -> ([ScheduleSend], [ScheduleSend])
        go _ [] acc = bimap reverse reverse acc
        go notVisited (l : ls) (cannotSend, canSend) =
            let notVisited' = Set.delete (getTxId l) notVisited
            in case dependsOnFutureTx notVisited' l of
                    True  -> go notVisited' ls (l : cannotSend, canSend)
                    False -> go notVisited' ls (cannotSend, l : canSend)

        -- | A 'ScheduleEvent' is @not@ independent and should not be sent
        -- over the wire if any of the inputs it consumes are mentioned in
        -- the 'Pending' set.
        dependsOnFutureTx :: Set Core.TxId -> ScheduleSend -> Bool
        dependsOnFutureTx notVisited (ScheduleSend _ txAux _) =
            let inputs = List.foldl' updateFn mempty $ (Core.taTx txAux) ^. Core.txInputs . to NonEmpty.toList
            in any (\tid -> isJust (M.lookup tid pending) && not (tid `Set.member` notVisited)) inputs

        getTxId :: ScheduleSend -> Core.TxId
        getTxId (ScheduleSend txId _ _) = txId

        updateFn :: [Core.TxId] -> Core.TxIn -> [Core.TxId]
        updateFn !acc (Core.TxInUnknown _ _)   = acc
        updateFn !acc (Core.TxInUtxo txHash _) = txHash : acc


-- | Extends the 'Schedule' with an extra set of [ScheduleSend] and
-- [ScheduleConfirm]. Useful to force dispatching in tests or simply as
-- an internal helper for the resubmission functions.
-- @N.B@ This is defined and exported as part of this module as it requires
-- internal knowledge of the internal state of the 'WalletSubmission'.
addToSchedule :: WalletSubmission m
              -> Slot
              -> [ScheduleSend]
              -> [ScheduleConfirm]
              -> WalletSubmission m
addToSchedule ws slot toSend toConfirm = ws & over (wsState . wssSchedule) override
    where
        override :: Schedule -> Schedule
        override (Schedule s n) =
            let newItem = ScheduleEvents toSend toConfirm
            in Schedule (IntMap.insertWith mappendEvents (castSlot slot) newItem s) n

        mappendEvents :: ScheduleEvents -> ScheduleEvents -> ScheduleEvents
        mappendEvents _ (ScheduleEvents send confirm) =
            ScheduleEvents (send <> toSend) (confirm <> toConfirm)


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
    in addToSchedule ws (mapSlot succ currentSlot) toSend mempty
    where
        toEntry :: (Core.TxId, Core.TxAux) -> ScheduleSend
        toEntry (txId, txAux) = ScheduleSend txId txAux (SubmissionCount 0)

        toSend :: [ScheduleSend]
        toSend =
            map toEntry (pending ^. pendingTransactions . fromDb . to M.toList)

-- Ready-to-use 'ResubmissionFunction's.

-- | A 'RetryPolicy' is simply a function which instruct the 'Schedule' when
-- to attempt resubmitting the given 'ScheduleEvent' item. It yields the
-- 'NextEvent' planned for a given 'Schedule'.
type RetryPolicy = SubmissionCount -> Slot -> NextEvent

-- | The next event a resubmission function will have to deal with.
data NextEvent = SendIn   !Slot
               -- ^ Schedule the event to happen at this 'Slot'.
               | CheckConfirmedIn !Slot
               -- ^ Check the transaction \"has made it\" in the given
               -- 'Slot', i.e. is not in the local 'Pending' set. If it is,
               -- it needs to be evicted.
               deriving (Show, Eq)

-- Internal combinators used to limit the number of retries of a 'RetryPolicy'
-- to an upper bound of 'MaxRetries' attempts.
limited :: MaxRetries -> (Slot -> Slot) -> RetryPolicy
limited maxRetries updateSlot submissionCount currentSlot
    | getSubmissionCount submissionCount < maxRetries = SendIn (updateSlot currentSlot)
    | otherwise = CheckConfirmedIn (updateSlot currentSlot)

type Exponent   = Double
type MaxRetries = Int

--
-- Stock retry policies inspired by the 'retry' package.
--

-- | Very simple policy which merely retries to resubmit the very next 'Slot',
-- up until 'MaxRetries' attempts.
constantRetry :: Int
              -- ^ The number of 'Slot's to \"skip\" every time we retry
              -> MaxRetries
              -> RetryPolicy
constantRetry n maxRetries = limited maxRetries (addSlot (Slot $ max 0 $ fromIntegral n))

-- | An exponential backoff policy, parametric over a maximum number of
-- 'MaxRetries' and an 'Exponent' for the backoff.
exponentialBackoff :: MaxRetries -> Exponent -> RetryPolicy
exponentialBackoff maxRetries exponent submissionCount currentSlot =
    let (delta :: Word) = fromIntegral ((floor (exponent ^^ (getSubmissionCount submissionCount))) :: Int)
    in  limited maxRetries (mapSlot ((+) delta)) submissionCount currentSlot

-- | A very customisable resubmitter which can be configured with different
-- retry policies.
defaultResubmitFunction :: forall m. Monad m
                        => (Core.TxAux -> m Bool)
                        -> RetryPolicy
                        -> ResubmissionFunction m
defaultResubmitFunction send retryPolicy currentSlot scheduled pendingSet oldSchedule = do
    foldlM updateFn (Set.empty, oldSchedule) scheduled
    where
        reschedule :: Either ScheduleSend ScheduleConfirm
                   -> Slot
                   -> IntMap ScheduleEvents
                   -> IntMap ScheduleEvents
        reschedule event targetSlot old =
            let (update, item) = case event of
                     Left ss  -> (\l _ -> l & over seToSend ((:) ss), ScheduleEvents [ss] mempty)
                     Right sc -> (\l _ -> l & over seToConfirm ((:) sc), ScheduleEvents mempty [sc])
            in IntMap.insertWith update (castSlot targetSlot) item old

        updateFn :: (Evicted, Schedule) -> ScheduleSend -> m (Evicted, Schedule)
        updateFn (evicted, (Schedule s nursery)) (ScheduleSend txId txAux submissionCount) = do
            -- We do not care about the result of 'send', our job
            -- is only to make sure we retrasmit the given transaction.
            -- It will be the blockchain to tell us (via adjustment to
            -- the local 'Pending' set) whether or not the transaction
            -- has been adopted.
            _ <- send txAux
            case needsEviction txs txId of
                 True -> return (Set.insert txId evicted, Schedule s nursery)
                 False -> case retryPolicy submissionCount currentSlot of
                             SendIn newSlot -> do
                                 let entry' = ScheduleSend txId txAux (incSubmissionCount submissionCount succ)
                                 return (evicted, Schedule (reschedule (Left entry') newSlot s) nursery)
                             CheckConfirmedIn newSlot ->
                                 return (evicted, Schedule (reschedule (Right $ ScheduleConfirm txId) newSlot s) nursery)

        txs :: M.Map Core.TxId Core.TxAux
        txs = pendingSet ^. pendingTransactions . fromDb

        -- For each transaction scheduled for confirmation, we check if it's
        -- still in the pending set. If not, it means it has been already adopted
        -- by the blockchain (and thus the wallet called 'remPending'), but if that's
        -- still there, it means there is no chance for it to be included in the blockchain
        -- and this submission layer is officially giving up on it.
        -- The wallet will likely reuse the returned 'Evicted' list of 'TxId's to
        -- amend its pending set (or later on call 'remPending').
        needsEviction :: M.Map Core.TxId Core.TxAux -> Core.TxId -> Bool
        needsEviction pendingTxs txId =
            isJust (M.lookup txId pendingTxs)

-- | Increments the 'SubmissionCount' by the supplied function.
incSubmissionCount :: SubmissionCount -> (Int -> Int) -> SubmissionCount
incSubmissionCount (SubmissionCount count) f =  SubmissionCount (f count)
