{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
-- We are exporting Lens' 'Getters', which has a redundant constraint on
-- \"contravariant\".
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Cardano.Wallet.Kernel.Submission (
    -- * Public API
      newWalletSubmission
    , addPending
    , remPending
    , remPendingById
    , tick
    , scheduledFor
    , tickSlot

    -- * Types and lenses
    , Cancelled
    , ResubmissionFunction
    , Schedule (..)
    , ScheduleEvents (..)
    , ScheduleSend (..)
    , NextEvent (..)
    , seToSend
    , seToConfirm
    , ScheduleEvictIfNotConfirmed (..)
    , Slot (..)
    , SubmissionCount (..)
    , WalletSubmission (..)
    , WalletSubmissionState (..)
    , MaxRetries
    , mapSlot
    , wsResubmissionFunction
    , getCurrentSlot
    , getSchedule

    -- * Internal useful function
    , addToSchedule
    , prependEvents

    -- * Internal functions exposed just to simplify testing
    , localPendingSet

    -- * Resubmitting things to the network
    , defaultResubmitFunction

    -- * Retry policies
    , constantRetry
    , exponentialBackoff
    ) where

import           Universum hiding (elems)

import           Control.Lens (Getter, anon, at, to)
import           Control.Lens.TH
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Formatting (bprint, (%))
import qualified Formatting as F
import           Formatting.Buildable (build)
import           Pos.Chain.Txp (topsortTxs)
import           Pos.Crypto.Hashing (WithHash (..))
import           Serokell.Util.Text (listJsonIndent, mapBuilder, pairF, tripleF)
import           Test.QuickCheck

import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId)
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import qualified Pos.Core.Txp as Txp

-- | Wallet Submission Layer
--
-- This module implements section 10 of the Wallet spec,
-- namely 'Transaction Submission'.
--
data WalletSubmission = WalletSubmission {
      _wsResubmissionFunction :: ResubmissionFunction
    -- ^ What is called 'rho' in the spec, a 'ResubmissionFunction' capable
    -- of retransmitting and rescheduling transactions.
    , _wsState                :: WalletSubmissionState
    -- ^ The internal (private) state of this layer. We do not export lenses
    -- to modify it, as that should be done only via this layer's public API.
    -- What we export are some 'Getter's to some interesting bits of the state,
    -- like the local 'Pending' set or the current slot.
    }

instance Buildable WalletSubmission where
    build ws = bprint ("WalletSubmission { rho = <function> , state = " % F.build % " }") (_wsState ws)

-- | The wallet internal state. Some useful invariant to check (possibly
-- via QuickCheck properties):
-- * Whatever we evict, it should not be in the pending set.
-- * Ff something is pending, it should also be in the schedule.
--   If this gets violated, some transaction might get stuck in the nursery forever.
-- * With a retry policy with MaxRetries == N there shouldn't be an entry in
--   the schedule with a SubmissionCount >= N
--
data WalletSubmissionState = WalletSubmissionState {
      _wssPendingMap  ::  M.Map HdAccountId Pending
    , _wssSchedule    ::  Schedule
    , _wssCurrentSlot :: !Slot
    }

instance Buildable WalletSubmissionState where
    build wss = bprint ("{ pendingMap = " % (F.later mapBuilder) %
                        ", scheduler  = " % F.build %
                        ", slot       = " % F.build %
                        " } "
                       ) (M.toList $ _wssPendingMap wss) (_wssSchedule wss) (getSlot $ _wssCurrentSlot wss)

-- | A 'Schedule' of events.
data Schedule = Schedule {
      _ssScheduled     :: IntMap ScheduleEvents
    -- ^ Despite modelled as in 'IntMap' it has to be intended
    -- as a mapping between 'Slot' and the list of transactions due that slot.
    -- The loss of precision in the cast is not a problem, and can be handled
    -- gracefully (crf. 'Slot' documentation).
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
data ScheduleSend = ScheduleSend HdAccountId Txp.TxId Txp.TxAux SubmissionCount deriving Eq

-- | A type representing an item (in this context, a transaction @ID@) which
-- needs to be checked against the blockchain for inclusion. In other terms,
-- we need to confirm that indeed the transaction identified by the given 'TxId' has
-- been adopted, i.e. it's not in the local pending set anymore.
data ScheduleEvictIfNotConfirmed = ScheduleEvictIfNotConfirmed HdAccountId Txp.TxId deriving Eq

-- | All the events we can schedule for a given 'Slot', partitioned into
-- 'ScheduleSend' and 'ScheduleEvictIfNotConfirmed'.
data ScheduleEvents = ScheduleEvents {
      _seToSend    :: [ScheduleSend]
    -- ^ A list of transactions which we need to send.
    , _seToConfirm :: [ScheduleEvictIfNotConfirmed]
    -- ^ A list of transactions which we need to check if they have been
    -- confirmed (i.e. adopted) by the blockchain.
    }

instance Semigroup ScheduleEvents where
    (ScheduleEvents s1 c1) <> (ScheduleEvents s2 c2) =
        ScheduleEvents (s1 <> s2) (c1 <> c2)

instance Buildable ScheduleSend where
    build (ScheduleSend accId txId _ s) = bprint ("ScheduleSend " % tripleF) (accId, txId, s)

instance Buildable [ScheduleSend] where
    build s = bprint (listJsonIndent 4) s

instance Buildable ScheduleEvictIfNotConfirmed where
    build (ScheduleEvictIfNotConfirmed accId txId) =
        bprint ("ScheduleEvictIfNotConfirmed " % pairF) (accId, txId)

instance Buildable [ScheduleEvictIfNotConfirmed] where
    build s = bprint (listJsonIndent 4) s

instance Buildable ScheduleEvents where
    build (ScheduleEvents ss sc) =
        bprint ("ScheduleEvents { toCheck = "   % F.build %
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

-- | Casts this 'Slot' to an 'Int'.
castSlot :: Slot -> Int
castSlot (Slot w) = fromIntegral w

-- | Adds two 'Slot's together.
addSlot :: Slot -> Slot -> Slot
addSlot (Slot w1) (Slot w2) = Slot (w1 + w2)

-- | Apply a function from 'Word' to 'Word' to the given 'Slot'.
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

-- | The 'Cancelled' map represents the transactions which needs to be
-- pruned from the local (and wallet) 'Pending' map.
type Cancelled = M.Map HdAccountId (Set Txp.TxId)

-- | A 'ResubmissionFunction' (@rho@ in the spec), parametrised by an
-- arbitrary @m@.
type ResubmissionFunction =  Slot
                          -- ^ The current slot. Handy to pass to this
                          -- function to reschedule transactions to some
                          -- other 'Slot' + N.
                          -> [ScheduleSend]
                          -- ^ Transactions which are due to be sent this 'Slot'.
                          -> Schedule
                          -- ^ The original 'Schedule'.
                          -> (Schedule, [Txp.TxAux])
                          -- ^ The new 'Schedule' together with the txs to
                          -- send.

makeLenses ''ScheduleEvents
makeLensesFor [("_ssScheduled", "ssScheduled")] ''Schedule
makeLenses ''WalletSubmission
makeLenses ''WalletSubmissionState

instance Buildable Schedule where
    build (Schedule ss nursery) =
        let elems = IntMap.toList ss
        in bprint ("Schedule { scheduled = " % (F.later mapBuilder) %
                           " , nursery   = " % (listJsonIndent 4)
                  ) elems nursery

instance Arbitrary SubmissionCount where
    arbitrary = SubmissionCount <$> choose (0, 255)

--
--
-- Public API, as written in the spec or mandated by real-world necessities.
--
--

-- | Creates a new 'WalletSubmission' layer from a 'ResubmissionFunction'.
-- The created 'WalletSubmission' will start at 'Slot' 0 with an empty
-- 'Schedule'.
newWalletSubmission :: ResubmissionFunction -> WalletSubmission
newWalletSubmission resubmissionFunction = WalletSubmission {
      _wsResubmissionFunction = resubmissionFunction
    , _wsState = newEmptyState
    }
    where
        newEmptyState :: WalletSubmissionState
        newEmptyState = WalletSubmissionState {
              _wssPendingMap  = M.empty
            , _wssCurrentSlot = Slot 0
            , _wssSchedule   = Schedule IntMap.empty mempty
            }

-- | A getter to the local pending set stored in this 'WalletSubmission'.
localPendingSet :: HdAccountId -> Getter WalletSubmission Pending
localPendingSet accId = pendingByAccId accId

-- | Gets the current 'Slot'.
getCurrentSlot :: Getter WalletSubmission Slot
getCurrentSlot = wsState . wssCurrentSlot

-- | Gets the current 'Schedule'.
getSchedule :: Getter WalletSubmission Schedule
getSchedule = wsState . wssSchedule

pendingByAccId :: HdAccountId -> Lens' WalletSubmission Pending
pendingByAccId accId =
  wsState . wssPendingMap . at accId . anon Pending.empty Pending.null

-- | Informs the 'WalletSubmission' layer about new 'Pending' transactions.
addPending :: HdAccountId -> Pending -> WalletSubmission -> WalletSubmission
addPending accId newPending ws =
    let ws' = ws & over (pendingByAccId accId)
                        (Pending.union newPending)
    in schedulePending accId newPending ws'

-- | Removes the input set of 'Txp.TxId' from the local 'WalletSubmission' pending set.
remPending :: Map HdAccountId (Set Txp.TxId)
           -> WalletSubmission
           -> WalletSubmission
remPending pendingMap ws =
    M.foldlWithKey' (\acc accId pendingSet ->
                        remPendingById accId pendingSet acc
                    ) ws pendingMap

remPendingById :: HdAccountId
               -> Set Txp.TxId
               -> WalletSubmission
               -> WalletSubmission
remPendingById accId ids ws =
    ws & over (pendingByAccId accId) (Pending.delete ids)

-- | A \"tick\" of the scheduler.
-- Returns the set transactions which needs to be droppped by the system as
-- they likely exceeded the submission count and they have no chance to be
-- adopted in a block.
-- @N.B.@ The returned 'WalletSubmission' comes with an already-pruned
-- local 'Pending' set, so it's not necessary to call 'remPending' afterwards.
tick :: WalletSubmission
     -- ^ The current 'WalletSubmission'.
     -> (Cancelled, [Txp.TxAux], WalletSubmission)
     -- ^ The set of transactions upper layers will need to drop, the
     -- transactions to be sent and the new 'WalletSubmission'.
tick ws =
    let wss         = ws  ^. wsState
        currentSlot = wss ^. wssCurrentSlot
        rho         = _wsResubmissionFunction ws
        pendingMap  = ws ^. wsState . wssPendingMap

        (scheduleSend, toConfirm, newSchedule) = tickSlot currentSlot ws
        (schedule', toSend) = rho currentSlot scheduleSend newSchedule
        evicted = evictedThisSlot toConfirm pendingMap
        newState = ws & wsState . wssSchedule    .~ schedule'
                      & wsState . wssCurrentSlot %~ mapSlot succ
        in (evicted, toSend, remPending evicted newState)
    where
        evictedThisSlot :: [ScheduleEvictIfNotConfirmed]
                        -> M.Map HdAccountId Pending
                        -> Cancelled
        evictedThisSlot toConfirm p =
            List.foldl' (checkConfirmed p) M.empty toConfirm

        checkConfirmed :: M.Map HdAccountId Pending
                       -> Cancelled
                       -> ScheduleEvictIfNotConfirmed
                       -> Cancelled
        checkConfirmed pending acc (ScheduleEvictIfNotConfirmed accId txId) =
            case M.lookup accId pending >>= Pending.lookup txId of
                 Just _  -> M.alter (alterFn txId) accId acc
                 Nothing -> acc

        alterFn :: Txp.TxId -> Maybe (Set Txp.TxId) -> Maybe (Set Txp.TxId)
        alterFn txId Nothing  = Just (Set.singleton txId)
        alterFn txId (Just s) = Just (Set.insert txId s)

--
--
-- Private API, used only internally or in tests.
--
--

-- | Convenient \"list-destructuring-style\" data accessor which returns
-- the next events scheduled for the input 'Slot' as well as the \"tail\" of the
-- 'Schedule'.
-- It doesn't perform any sophisticated logic on the actual events which will
-- be eventually sent, nor tries to update the nursery. That is performed
-- specifically by the 'tickSlot' function.
scheduledFor :: Slot -> Schedule -> (ScheduleEvents, Schedule)
scheduledFor currentSlot s@(Schedule schedule nursery) =
    case IntMap.lookup (castSlot currentSlot) schedule of
         Nothing -> (ScheduleEvents mempty mempty, s)
         Just candidates ->
             (candidates, Schedule (IntMap.delete (castSlot currentSlot) schedule) nursery)

-- | Returns a set of 'Pending' transactions which are due in the given
-- 'Slot', together with the ones which needs to be checked for eviction and
-- the new 'Schedule'.
-- This is the workhorse of the entire layer, as it's its responsibility
-- to look at the input 'Schedule' and determine which transactions are due to
-- be send this 'Slot' and which needs to go into the nursery. It does so by
-- topologically sorting all the potential candidates (i.e. transactions still
-- in the 'Pending' set) and then trying to establish which of them are \"independent\"
-- from future transactions, i.e they can be send in the given 'Slot' without being
-- rejected by neighbours node as they have a direct depedency on some other
-- transaction yet to be schedule/sent.
tickSlot :: Slot
         -- ^ The current 'Slot'.
         -> WalletSubmission
         -- ^ The 'WalletSubmissionState'.
         -> ([ScheduleSend], [ScheduleEvictIfNotConfirmed], Schedule)
         -- ^ All the scheduled transactions together with the new
         -- , updated 'Schedule'.
tickSlot currentSlot ws =
    let (allEvents, schedule) = scheduledFor currentSlot (ws ^. wsState . wssSchedule)
        scheduledCandidates = filterNotConfirmed (allEvents ^. seToSend <> nursery schedule)
        localPending = ws ^. wsState . wssPendingMap
        -- NOTE Here we are sorting @all@ the pending transactions in @all@
        -- the accounts, instead of sorting each \"pool\" individually. This helps
        -- with inter-account transactions, as per problem described in [CBR-308].
        topSorted  = topsortTxs toTx scheduledCandidates
    in case topSorted of
            Nothing     ->
                let msg = "tickSlot, invariant violated: a loop was detected " <>
                          "while trying to top-sort the pending transactions " <>
                          "scheduled for slot " <> show currentSlot <> ". This " <>
                          "indicates a bug in the transaction creation process."
                in error msg
            Just sorted ->
                let (send, cannotSend) = partitionSendable localPending sorted
                    newSchedule = schedule { _ssUnsentNursery = cannotSend }
                in (send, allEvents ^. seToConfirm, newSchedule)
    where
        nursery :: Schedule -> [ScheduleSend]
        nursery (Schedule _ n) = n

        toTx :: ScheduleSend -> WithHash Txp.Tx
        toTx (ScheduleSend _ txId txAux _) =  WithHash (Txp.taTx txAux) txId

        pendingTxs :: HdAccountId -> Pending
        pendingTxs accId = ws ^. pendingByAccId accId

        -- Filter the transactions not appearing in the local pending set
        -- anymore, as they have been adopted by the blockchain and we should
        -- stop resubmitting them.
        filterNotConfirmed :: [ScheduleSend] -> [ScheduleSend]
        filterNotConfirmed =
            filter (\(ScheduleSend accId txId _ _) ->
                   isJust (Pending.lookup txId (pendingTxs accId)))

-- | Similar to 'Data.List.partition', but partitions the input 'ScheduleSend'
-- list into events which can be sent this 'Slot', and other which needs to
-- end up in the nursery as they are depedent on future transactions.
partitionSendable :: M.Map HdAccountId Pending
                  -- ^ The list of all the 'Pending' set, classified by account.
                  -> [ScheduleSend]
                  -- ^ A @topologically sorted@ list of transactions scheduled
                  -- for being sent.
                  -> ([ScheduleSend], [ScheduleSend])
partitionSendable pendingSets xs =
    go xs ((Set.empty, mempty), mempty)
    where
        go :: [ScheduleSend]
           -> ((Set Txp.TxId, [ScheduleSend]), [ScheduleSend])
           -> ([ScheduleSend], [ScheduleSend])
        go [] acc = bimap (reverse . snd) reverse acc
        go (l : ls) ((accCanSendIds, accCanSend), accCannotSend) =
            case dependsOnFutureTx accCanSendIds l of
                 True  -> go ls ((accCanSendIds, accCanSend), l : accCannotSend)
                 False -> go ls ((Set.insert (getTxId l) accCanSendIds, l : accCanSend), accCannotSend)

        -- | A 'ScheduleEvent' is @not@ independent and should not be sent
        -- over the wire if any of the inputs it consumes are mentioned in
        -- the 'Pending' set.
        dependsOnFutureTx :: Set Txp.TxId -> ScheduleSend -> Bool
        dependsOnFutureTx canSendIds (ScheduleSend accId _ txAux _) =
            let inputs = List.foldl' updateFn mempty $ (Txp.taTx txAux) ^. Txp.txInputs . to NonEmpty.toList
            in any (\tid -> isJust (lookupTx tid accId) && not (tid `Set.member` canSendIds)) inputs

        getTxId :: ScheduleSend -> Txp.TxId
        getTxId (ScheduleSend _ txId _ _) = txId

        lookupTx :: Txp.TxId -> HdAccountId -> Maybe Txp.TxAux
        lookupTx tid accId =
            M.lookup accId pendingSets >>=
            Pending.lookup tid

        updateFn :: [Txp.TxId] -> Txp.TxIn -> [Txp.TxId]
        updateFn !acc (Txp.TxInUnknown _ _)   = acc
        updateFn !acc (Txp.TxInUtxo txHash _) = txHash : acc

-- | Extends the 'Schedule' with an extra set of [ScheduleSend] and
-- [ScheduleEvictIfNotConfirmed]. Useful to force dispatching in tests or simply as
-- an internal helper for the resubmission functions.
-- @N.B@ This is defined and exported as part of this module as it requires
-- internal knowledge of the internal state of the 'WalletSubmission'.
addToSchedule :: WalletSubmission
              -> Slot
              -> [ScheduleSend]
              -> [ScheduleEvictIfNotConfirmed]
              -> WalletSubmission
addToSchedule ws slot toSend toConfirm =
    ws & over (wsState . wssSchedule . ssScheduled) prepend
    where
        prepend :: IntMap ScheduleEvents -> IntMap ScheduleEvents
        prepend = prependEvents slot (ScheduleEvents toSend toConfirm)

-- | Schedule the full list of pending transactions.
-- The transactions will be scheduled immediately in the next 'Slot'.
schedulePending :: HdAccountId
                -> Pending
                -> WalletSubmission
                -> WalletSubmission
schedulePending accId pending ws =
    let currentSlot = ws ^. wsState . wssCurrentSlot
    in addToSchedule ws (mapSlot succ currentSlot) toSend mempty
    where
        toEntry :: (Txp.TxId, Txp.TxAux) -> ScheduleSend
        toEntry (txId, txAux) = ScheduleSend accId txId txAux (SubmissionCount 0)

        toSend :: [ScheduleSend]
        toSend = map toEntry (Pending.toList pending)

--
--
-- Ready-to-use 'ResubmissionFunction's.
--
--

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
-- We don't want to throw an error in case we end up a case where
-- getSubmissionCount submissionCount > maxRetries, because different
-- ResubmissionFunctions can be configured with different 'RetryPolicy'es, and
-- those can have a more stringent limit on a policy applied at until a given
-- moment, so it's still possible to have elements in the schedule with a
-- 'SubmissionCount' larger than the 'MaxRetries', and calling the 'retryPolicy'
-- would cause an error. Having a lenient @otherwise@ case solves this.
limited :: MaxRetries -> (Slot -> Slot) -> RetryPolicy
limited maxRetries updateSlot submissionCount currentSlot
    | getSubmissionCount submissionCount  < maxRetries = SendIn (updateSlot currentSlot)
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
defaultResubmitFunction :: RetryPolicy
                        -> ResubmissionFunction
defaultResubmitFunction retryPolicy currentSlot scheduled oldSchedule =
    -- We do not care about the result of 'send', our job
    -- is only to make sure we retrasmit the given transaction.
    -- It will be the blockchain to tell us (via adjustment to
    -- the local 'Pending' set) whether or not the transaction
    -- has been adopted. Users can tweak any concurrency behaviour by
    -- tucking such behaviour in the 'send' function itself.
    let toSend = map (\(ScheduleSend _ _ txAux _) -> txAux) scheduled
    in (List.foldl' updateFn oldSchedule scheduled, toSend)
    where
        updateFn :: Schedule -> ScheduleSend -> Schedule
        updateFn (Schedule s nursery) (ScheduleSend accId txId txAux submissionCount) =
            let submissionCount' = incSubmissionCount submissionCount succ
                (targetSlot, newEvent) = case retryPolicy submissionCount' currentSlot of
                  SendIn newSlot ->
                      (newSlot, ScheduleEvents [ScheduleSend accId txId txAux submissionCount'] mempty)
                  CheckConfirmedIn newSlot ->
                      (newSlot, ScheduleEvents mempty [ScheduleEvictIfNotConfirmed accId txId])
            in Schedule (prependEvents targetSlot newEvent s) nursery

-- | Prepends all the input 'ScheduleEvents' (i.e. the 'ScheduleSend', and
-- 'ScheduleEvictIfNotConfirmed' contained within) at the beginning of each respective
-- collection.
prependEvents :: Slot
              -> ScheduleEvents
              -> IntMap ScheduleEvents
              -> IntMap ScheduleEvents
prependEvents targetSlot events old =
    IntMap.alter alterFn (castSlot targetSlot) old
    where
        alterFn :: Maybe ScheduleEvents -> Maybe ScheduleEvents
        alterFn Nothing          = Just events
        alterFn (Just oldEvents) = Just (events <> oldEvents)

-- | Increments the 'SubmissionCount' by the supplied function.
incSubmissionCount :: SubmissionCount -> (Int -> Int) -> SubmissionCount
incSubmissionCount (SubmissionCount count) f =  SubmissionCount (f count)
