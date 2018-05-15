module Cardano.Wallet.Kernel.Submission (
    -- * Public API
      newWalletSubmission
    , addPending
    , remPending
    , tick

    -- * Types and lenses
    , WalletSubmission
    , ResubmissionFunction
    , Schedule (..)
    , wsResubmissionFunction
    , wsState
    , wssPendingSet

    -- * Ready to use resubmission functions
    , constantResubmission

    -- * Testing utilities
    , unsafeSchedule
    , genWalletSubmissionState
    , genWalletSubmission
    ) where

import           Universum

import qualified Control.Concurrent.Async as Async
import           Control.Lens (to)
import           Control.Lens.TH
import qualified Data.List as List
import qualified Data.Map as M
import           Data.Text.Buildable (build)
import           Formatting (bprint, (%))
import qualified Formatting as F
import           Serokell.Util.Text (mapBuilder, pairF)
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
data WalletSubmission = WalletSubmission {
      _wsResubmissionFunction :: ResubmissionFunction
    , _wsState                :: WalletSubmissionState
    }

instance Buildable WalletSubmission where
    build ws = bprint ("WalletSubmission <rho> " % F.build) (_wsState ws)

data WalletSubmissionState = WalletSubmissionState {
      _wssPendingSet :: Pending
    , _wssScheduler  :: SubmissionScheduler
    , _wssDiffusion  :: WalletDiffusion
    }

instance Buildable WalletSubmissionState where
    build wss = bprint ("{ pendingSet = " % F.build %
                        ", scheduler  = " % F.build %
                        ", diffusion  = <diffusion> } "
                       ) (_wssPendingSet wss) (_wssScheduler wss)

data SubmissionScheduler = SubmissionScheduler {
    _ssScheduled :: M.Map Core.TxId Schedule
    }

type TTL = Int

defaultTTL :: TTL
defaultTTL = 255

newtype Schedule = Schedule (Core.FlatSlotId, TTL)

instance Buildable Schedule where
    build (Schedule s) = bprint pairF s

type ResubmissionFunction =  Core.FlatSlotId
                          -- ^ The current slot
                          -> Pending
                          -- ^ Transactions which are due
                          -> WalletSubmissionState
                          -> IO (Pending, WalletSubmissionState)

makeLenses ''SubmissionScheduler
makeLenses ''WalletSubmission
makeLenses ''WalletSubmissionState

instance Buildable SubmissionScheduler where
    build ss =
        let elems = ss ^. ssScheduled . to M.toList
        in bprint (F.later mapBuilder) elems


instance Arbitrary Schedule where
    arbitrary = do
        s <- (,) <$> arbitrary
                 <*> choose (-10, defaultTTL) -- The system should be able to deal with negative TTLs.
        pure $ Schedule s

instance Arbitrary SubmissionScheduler where
    arbitrary = SubmissionScheduler <$> arbitrary

genWalletSubmissionState :: WalletDiffusion -> Gen WalletSubmissionState
genWalletSubmissionState walletDiffusion =
    WalletSubmissionState <$> genPending (Core.ProtocolMagic 0)
                          <*> arbitrary
                          <*> pure walletDiffusion

genWalletSubmission :: WalletDiffusion
                    -> ResubmissionFunction
                    -> Gen WalletSubmission
genWalletSubmission walletDiffusion rho =
    WalletSubmission <$> pure rho <*> genWalletSubmissionState walletDiffusion

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
addPending :: WalletSubmission -> Pending -> WalletSubmission
addPending ws updatedPending =
    ws & over (wsState . wssPendingSet) (unionPending updatedPending)

-- | Removes the input 'Pending' from the local 'WalletSubmission' pending set.
remPending :: WalletSubmission -> Pending -> WalletSubmission
remPending ws updatedPending =
    ws & over (wsState . wssPendingSet) (flip differencePending updatedPending)

-- | A \"tick\" of the scheduler.
-- Returns the set transactions which needs to be droppped by the system as
-- they likely exceeded the submission count and they have no chance to be
-- adopted in a block.
-- @N.B.@ The returned 'WalletSubmission' comes with an already-pruned
-- local 'Pending' set.
--
-- FIXME(adn) Make it run in any 'Monad' @m@.
tick :: WalletSubmission
     -- ^ The current 'WalletSubmission'.
     -> Core.FlatSlotId
     -- ^ The current 'SlotId', flattened.
     -> IO (Pending, WalletSubmission)
     -- ^ The set of transactions upper layers will need to drop, together
     -- with the new 'WalletSubmission'.
tick ws currentSlot = do
    let wss       = ws ^. wsState
        rho       = _wsResubmissionFunction ws
        dueNow    = dueThisSlot currentSlot wss
    (toPrune, wss') <- rho currentSlot dueNow wss
    return (toPrune , remPending (ws & wsState .~ wss') toPrune)

-- | Overrides the 'SubmissionScheduler' with an input transaction and a
-- custom 'Schedule'. Useful to force dispatching, especially in tests.
unsafeSchedule :: WalletSubmission
               -> Core.TxId
               -> Core.TxAux
               -> Schedule
               -> WalletSubmission
unsafeSchedule ws txId txAux customSchedule =
    ws & over (wsState . wssPendingSet) (addOneToPending txId txAux)
       . over (wsState . wssScheduler) overrideSchedule
    where
        overrideSchedule :: SubmissionScheduler -> SubmissionScheduler
        overrideSchedule (SubmissionScheduler s) =
            SubmissionScheduler (M.insert txId customSchedule s)

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

differencePending :: Pending -> Pending -> Pending
differencePending (Pending new) (Pending old) =
    Pending (M.difference <$> new <*> old)

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


