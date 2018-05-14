module Cardano.Wallet.Kernel.Submission (
    -- * Public API
      newWalletSubmission
    , addPending
    , remPending
    , tick

    -- * Types and lenses
    , WalletSubmission
    , ResubmissionFunction
    , wsResubmissionFunction

    -- * Ready to use resubmission functions
    , constantResubmission
    ) where

import           Universum

import qualified Control.Concurrent.Async as Async
import           Control.Lens (to)
import           Control.Lens.TH
import qualified Data.List as List
import qualified Data.Map as M

import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (Pending (..), emptyPending, pendingTransactions)
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

-- | Wallet Submission Layer
--
-- This module implements section 9 of the Wallet spec,
-- namely 'Transaction Submission'.
--
data WalletSubmission = WalletSubmission {
      _wsResubmissionFunction :: ResubmissionFunction
    , _wsState                :: WalletSubmissionState
    }

data WalletSubmissionState = WalletSubmissionState {
      _wssPendingSet :: Pending
    , _wssScheduler  :: SubmissionScheduler
    , _wssDiffusion  :: WalletDiffusion
    }

data SubmissionScheduler = SubmissionScheduler {
    _ssScheduled :: M.Map Core.TxId Schedule
    }

type TTL = Int

defaultTTL :: TTL
defaultTTL = 255

newtype Schedule = Schedule (Core.FlatSlotId, TTL)

type ResubmissionFunction =  Core.FlatSlotId
                          -- ^ The current slot
                          -> Pending
                          -- ^ Transactions which are due
                          -> WalletSubmissionState
                          -> IO (Pending, WalletSubmissionState)

makeLenses ''SubmissionScheduler
makeLenses ''WalletSubmission
makeLenses ''WalletSubmissionState

--
-- Public API, as written in the spec
--

newWalletSubmission :: ResubmissionFunction
                    -> WalletDiffusion
                    -> WalletSubmission
newWalletSubmission resubmissionFunction diffusion = WalletSubmission {
      _wsResubmissionFunction = resubmissionFunction
    , _wsState = newEmptyState
    }
    where
        newEmptyState :: WalletSubmissionState
        newEmptyState = WalletSubmissionState {
              _wssPendingSet = emptyPending
            , _wssScheduler  = SubmissionScheduler M.empty
            , _wssDiffusion  = diffusion
            }

-- | Informs the 'WalletSubmission' layer of the new, updated 'PendingSet'.
addPending :: Pending -> WalletSubmission -> WalletSubmission
addPending updatedPending ws =
    ws & over (wsState . wssPendingSet) (unionPending updatedPending)

remPending :: Pending -> WalletSubmission -> WalletSubmission
remPending updatedPending ws =
    ws & over (wsState . wssPendingSet) (intersectPending updatedPending)

-- | A \"tick\" of the scheduler.
-- Returns the set transactions which needs to be droppped by the system as
-- they likely exceeded the submission count and they have no chance to be
-- adopted in a block.
-- FIXME(adn) Make it run in any 'Monad' @m@.
tick :: Core.FlatSlotId
     -- ^ The current 'SlotId', flattened.
     -> WalletSubmission
     -- ^ The current 'WalletSubmission'.
     -> IO (Pending, WalletSubmission)
     -- ^ The set of transactions upper layers will need to drop, together
     -- with the new 'WalletSubmission'.
     -- N.B. The returned 'WalletSubmission' comes with an already-pruned
     -- local 'Pending' set.
tick currentSlot ws = do
    let wss       = ws ^. wsState
        rho       = _wsResubmissionFunction ws
        dueNow    = dueThisSlot currentSlot wss
    (toPrune, wss') <- rho currentSlot dueNow wss
    return (toPrune , remPending toPrune (ws & wsState .~ wss'))


--
-- Internal API
--

-- | Returns a set of 'Pending' transactions which are due in the given
-- 'FlatSlotId'.
dueThisSlot :: Core.FlatSlotId
            -- ^ The current 'SlotId', flattened.
            -> WalletSubmissionState
            -- ^ The 'WalletSubmissionState'.
            -> Pending
            -- ^ The filtered set of 'Pending' which are due in the input slot.
dueThisSlot currentSlot wss =
    let scheduled  = wss ^. wssScheduler . to _ssScheduled
        pending    = wss ^. wssPendingSet
    in pending & over pendingTransactions (fmap (M.filterWithKey (dueNowOrExpired scheduled)))
    where
        dueNowOrExpired :: M.Map Core.TxId Schedule
                        -> Core.TxId
                        -> Core.TxAux
                        -> Bool
        dueNowOrExpired scheduled txId _ =
            case M.lookup txId scheduled of
                 Nothing                   -> False
                 Just (Schedule (slot, _)) -> slot <= currentSlot


-- TODO(adn): Better placed into module 'Cardano.Wallet.Kernel.DB.Spec'?
unionPending :: Pending -> Pending -> Pending
unionPending (Pending new) (Pending old) =
    Pending (M.union <$> new <*> old)

intersectPending :: Pending -> Pending -> Pending
intersectPending (Pending new) (Pending old) =
    Pending (M.intersection <$> new <*> old)

addOneToPending :: Core.TxId -> Core.TxAux -> Pending -> Pending
addOneToPending txId aux (Pending p) = Pending (M.insert txId aux <$> p)

-- Ready-to-use 'ResubmissionFunction's.

-- | A very lenient resubmitter which blindly schedules each pending transaction
-- to be resubmitted the next slot.
-- NOTE(adn): In this function we are submitting all the due transactions first,
-- and only later check if we exceeded the TTL. This is slightly more taxing
-- on the network layer but not a real problem: in the worst case scenario we
-- would simply send a transaction to the neighbours twice.
-- NOTE(adn): Probably bollocks code.
constantResubmission :: ResubmissionFunction
constantResubmission currentSlot (Pending due) oldState = do
    let diffusion = oldState ^. wssDiffusion
        scheduled = oldState ^. wssScheduler . ssScheduled
    results <- Async.mapConcurrently (trySubmit diffusion) (due ^. fromDb . to M.toList)
    let (toPrune, scheduled') = List.foldl' updateFn (emptyPending, scheduled) results
    return (toPrune, oldState & (wssScheduler . ssScheduled) .~ scheduled')
    where
        trySubmit :: WalletDiffusion
                  -> (Core.TxId, Core.TxAux)
                  -> IO (Core.TxId, Core.TxAux, Either SomeException Bool)
        trySubmit diffusion (txId, aux) = do
            newJob <- Async.async (walletSendTx diffusion aux)
            (txId,aux,) <$> Async.waitCatch newJob

        updateFn :: (Pending, M.Map Core.TxId Schedule)
                 -> (Core.TxId, Core.TxAux, Either SomeException Bool)
                 -> (Pending, M.Map Core.TxId Schedule)
        updateFn (toPrune, acc) (txId, txAux, result) =
            case M.lookup txId acc of
                 -- If we cannot find the entry in the scheduler, it means
                 -- this is the first time we see this transaction and we need
                 -- to formally \"list it\".
                 Nothing ->
                     case result of
                         Left _      -> (toPrune, M.insert txId (Schedule (currentSlot + 2, defaultTTL)) acc)
                         Right False -> (toPrune, M.insert txId (Schedule (currentSlot + 1, defaultTTL)) acc)
                         Right True  -> (addOneToPending txId txAux toPrune, acc)
                 Just (Schedule (_, ttl)) | ttl <= 0 ->
                     (addOneToPending txId txAux toPrune, M.delete txId acc)
                 Just (Schedule (oldSlot, oldTTL)) ->
                     case result of
                          Left _      -> (toPrune, M.adjust (const $ Schedule (oldSlot + 2, oldTTL - 1)) txId acc)
                          Right False -> (toPrune, M.adjust (const $ Schedule (oldSlot + 1, oldTTL - 1)) txId acc)
                          Right True  -> (addOneToPending txId txAux toPrune, M.delete txId acc)


