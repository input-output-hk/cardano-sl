module Cardano.Wallet.Kernel.Submission (
    -- * Public API
      newWalletSubmission
    , addPending
    , remPending
    , tick

    -- * Types and lenses
    , WalletSubmission
    , wsResubmissionFunction
    ) where

import           Universum

import           Control.Lens.TH
import           Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as PSQ
import qualified Data.Map as M

import           Cardano.Wallet.Kernel.DB.Spec (Pending (..), emptyPending)
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
    _ssSubmissionQueue :: HashPSQ Core.TxId SubmissionPriority Core.TxAux
    }

-- | TODO(adn) Write better instances.
newtype SubmissionPriority =
  SubmissionPriority (Core.SlotId, SubmissionCount) deriving (Eq, Ord)

type SubmissionCount = Int

-- FIXME(adn) Just an ill-named placeholder.
type ToResubmit = ()

type ResubmissionFunction = (ToResubmit, SubmissionScheduler) -> Pending

makeLenses ''WalletSubmission
makeLensesFor [("_wssPendingSet", "wssPendingSet")] ''WalletSubmissionState

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
            , _wssScheduler  = SubmissionScheduler PSQ.empty
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
tick :: WalletSubmission -> (Pending, WalletSubmission)
tick ws = (emptyPending, ws)

--
-- Internal API
--

-- TODO(adn): Better placed into module 'Cardano.Wallet.Kernel.DB.Spec'?
unionPending :: Pending -> Pending -> Pending
unionPending (Pending new) (Pending old) =
    Pending (M.union <$> new <*> old)

intersectPending :: Pending -> Pending -> Pending
intersectPending (Pending new) (Pending old) =
    Pending (M.intersection <$> new <*> old)

