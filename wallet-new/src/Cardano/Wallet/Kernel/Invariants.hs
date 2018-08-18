-- | Internal invariants (used during testing)
module Cardano.Wallet.Kernel.Invariants (
    checkInvariantSubmission
  ) where

import           Universum

import qualified Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import           Serokell.Util (mapJson)

import           Pos.Core.Txp (TxId)

import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.Read as DB
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.Internal (PassiveWallet,
                     walletSubmission)
import           Cardano.Wallet.Kernel.Read (DB, getWalletSnapshot)
import           Cardano.Wallet.Kernel.Submission (WalletSubmission)
import qualified Cardano.Wallet.Kernel.Submission as WS

{-------------------------------------------------------------------------------
  Testing
-------------------------------------------------------------------------------}

-- | Check invariant for the submission layer
--
-- The wallet's own pending set and the submission layer's may be temporarily
-- out of sync. For example, the wallet may update its own pending set before
-- notifying the submission layer after applying a block. Nonetheless, at most
-- times they should match: a transaction should be  in the submission layer's
-- pending set if and only if it is in the wallet's.
checkInvariantSubmission :: PassiveWallet -> IO ()
checkInvariantSubmission pw = checkInvariant =<<
    invariantSubmission <$> getWalletSnapshot pw
                        <*> (readMVar (pw ^. walletSubmission))

invariantSubmission :: DB -> WalletSubmission -> Either InvariantViolated ()
invariantSubmission db ws =
    if sanitize dbPending == sanitize wsPending
      then Right ()
      else Left $ SubmissionLayerOutOfSync dbPending wsPending
  where
    dbPending, wsPending :: Map HdAccountId Pending
    dbPending = DB.pendingByAccount db
    wsPending = WS.pendingByAccount ws

    sanitize :: Map HdAccountId Pending -> Map HdAccountId (Set TxId)
    sanitize = Map.filter (not . Set.null)
             . map Pending.transactionIds

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

checkInvariant :: Either InvariantViolated () -> IO ()
checkInvariant (Left err) = throwM err
checkInvariant (Right ()) = return ()

data InvariantViolated =
    -- | The database and the submission layer disagree on the pending set
    SubmissionLayerOutOfSync (Map HdAccountId Pending) (Map HdAccountId Pending)

instance Show InvariantViolated where
  show = formatToString build

instance Exception InvariantViolated

instance Buildable InvariantViolated where
  build (SubmissionLayerOutOfSync dbPending wsPending) = bprint
    ( "SubmissionLayerOutOfSync "
    % "{ dbPending = " % mapJson
    % ", wsPending = " % mapJson
    % "}"
    )
    dbPending
    wsPending
