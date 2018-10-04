module Pos.Wallet.Web.Tracking.Types
       ( SyncQueue
       , SyncRequest  (..)
       , TrackingOperation (..)
       , SyncError (..)
       , SyncResult
       , BlockLockMode
       , WalletTrackingEnv

       -- * Combinators & smart constructors
       , newRestoreRequest
       , newSyncRequest
       , submitSyncRequest
       ) where

import           Universum

import           Control.Concurrent.STM (TQueue, writeTQueue)

import           Pos.DB.Class (MonadDBRead (..))
import           Pos.Infra.Slotting (MonadSlotsData)
import           Pos.Infra.StateLock (StateLock)
import           Pos.Util (HasLens (..))

import           Pos.Util.Wlog (WithLogger)
import           Pos.Wallet.Web.ClientTypes (CId, Wal)
import           Pos.Wallet.Web.Error.Types (WalletError (..))
import qualified Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.State.State (RestorationBlockDepth (..))
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials)

type BlockLockMode ctx m =
     ( WithLogger m
     , MonadDBRead m
     , MonadReader ctx m
     , HasLens StateLock ctx StateLock
     , MonadMask m
     )

type WalletTrackingEnv ctx m =
     ( WS.WalletDbReader ctx m
     , MonadSlotsData ctx m
     , WithLogger m
     , MonadThrow m
     , MonadDBRead m
     , BlockLockMode ctx m
     )

-- | A 'SyncQueue' is a bounded queue where we store incoming 'SyncRequest', and
-- we process them asynchronously.
type SyncQueue = TQueue SyncRequest

-- | A 'SyncRequest' model the interaction the user (or more generally the system)
-- has with the wallet.
data SyncRequest = SyncRequest {
      srOperation   :: !TrackingOperation
    , srCredentials :: !WalletDecrCredentials
    }

data TrackingOperation = SyncWallet
                       -- ^ Sync the wallet @history@ and @balance@ with the blockchain as
                       -- in "what have I missed while offline". Start from the latest 'HeaderHash'
                       -- the given wallet is synced with, but do not process more than 'BlockBatchSize'
                       -- block.
                       | RestoreWallet RestorationBlockDepth
                       -- ^ Restore the full wallet @history@ as in "restoring a wallet
                       -- from seed." for the whole blockchain (starting from the genesis block),
                       -- but track UTXO changes for blocks coming _after_ the 'RestorationBlockDepth'.
                       deriving (Eq, Show)

newRestoreRequest :: WalletDecrCredentials -> RestorationBlockDepth -> SyncRequest
newRestoreRequest creds rhh = SyncRequest (RestoreWallet rhh) creds

newSyncRequest :: WalletDecrCredentials -> SyncRequest
newSyncRequest creds = SyncRequest SyncWallet creds

-- | An enumerations of the errors the syncing process can yield.
-- Such errors are handled internally at the moment, where an error is just logged and flushed away,
-- but could be later exposed to the frontend to offer more fine-grained diagnostics.
data SyncError = GenesisBlockHeaderNotFound
               -- ^ Fetching the genesis block header failed.
               | GenesisHeaderHashNotFound
               -- ^ Fetching the genesis header hash failed.
               | NoSyncStateAvailable (CId Wal)
               -- ^ There was no sync state available for this wallet.
               | RestorationInvariantViolated (CId Wal) RestorationBlockDepth RestorationBlockDepth
               -- ^ When processing a 'SyncState' inside 'syncWalletWithBlockchain', the externally-provided
               -- 'RestorationBlockDepth' was different from the one stored in the model.
               | StateTransitionNotAllowed (CId Wal) TrackingOperation RestorationBlockDepth
               -- ^ When processing a 'SyncState' inside 'syncWalletWithBlockchain', the external request was
               -- a restore, but the internal state of the wallet represented an already-restored wallet.
               | NotSyncable (CId Wal) WalletError
               -- ^ The given wallet cannot be synced due to an unexpected error.
               -- The routine was not even started.
               | SyncFailed  (CId Wal) SomeException
               -- ^ The sync process failed abruptly during the sync process.

-- | A 'SyncResult' represents the possible outcomes of a syncing operation: it can either
-- fail with a 'SyncError', or succeed by actualising the source wallet up to a particular 'BlockHeader'.
type SyncResult = Either SyncError ()

-- | Submit a 'SyncRequest' to the asynchronous worker.
submitSyncRequest :: ( MonadIO m
                     , MonadReader ctx m
                     , HasLens SyncQueue ctx SyncQueue
                     ) => SyncRequest -> m ()
submitSyncRequest syncRequest = do
    requestQueue <- view (lensOf @(TQueue SyncRequest))
    atomically $ writeTQueue requestQueue syncRequest
