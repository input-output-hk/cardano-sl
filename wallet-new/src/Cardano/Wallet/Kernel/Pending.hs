-- | Deal with pending transactions
module Cardano.Wallet.Kernel.Pending (
    newPending
  , newForeign
  , cancelPending
  , NewPendingError
  ) where

import           Universum hiding (State)

import           Control.Concurrent.MVar (modifyMVar_)
import           Data.Acid.Advanced (update')

import           Pos.Core.Txp (TxAux (..))

import           Cardano.Wallet.Kernel.DB.AcidState (CancelPending (..),
                     NewForeign (..), NewForeignError, NewPending (..),
                     NewPendingError)
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta, putTxMeta)
import           Cardano.Wallet.Kernel.Internal
import           Cardano.Wallet.Kernel.Submission (Cancelled, addPending)

{-------------------------------------------------------------------------------
  Submit pending transactions
-------------------------------------------------------------------------------}

-- | Submit a new pending transaction
--
-- Will fail if the HdAccountId does not exist or if some inputs of the
-- new transaction are not available for spending.
--
-- If the pending transaction is successfully added to the wallet state, the
-- submission layer is notified accordingly.
newPending :: ActiveWallet -> HdAccountId -> TxAux -> Maybe TxMeta ->  IO (Either NewPendingError ())
newPending ActiveWallet{..} accountId tx mbMeta = do
    res <- update' (walletPassive ^. wallets) $ NewPending accountId (InDb tx)
    case res of
        Left e -> return (Left e)
        Right () -> do
            case mbMeta of
                Just meta -> putTxMeta (walletPassive ^. walletMeta) meta
                Nothing   -> pure ()
            modifyMVar_ walletSubmission (return . addPending accountId (Pending.singleton tx))
            return $ Right ()

-- | Submit new foreign transaction
--
-- A foreign transaction is a transaction that transfers funds from /another/
-- wallet to this one.
newForeign :: ActiveWallet -> HdAccountId -> TxAux -> TxMeta -> IO (Either NewForeignError ())
newForeign ActiveWallet{..} accountId tx meta = do
    res <- update' (walletPassive ^. wallets) $ NewForeign accountId (InDb tx)
    case res of
        Left e -> return (Left e)
        Right () -> do
            putTxMeta (walletPassive ^. walletMeta) meta
            modifyMVar_ walletSubmission (return . addPending accountId (Pending.singleton tx))
            return $ Right ()

-- | Cancel a pending transaction
--
-- NOTE: This gets called in response to events /from/ the wallet submission
-- layer, so we shouldn't be notifying the submission in return here.
--
-- This removes the transaction from either pending or foreign.
cancelPending :: PassiveWallet -> Cancelled -> IO ()
cancelPending passiveWallet cancelled =
    update' (passiveWallet ^. wallets) $ CancelPending (fmap InDb cancelled)
