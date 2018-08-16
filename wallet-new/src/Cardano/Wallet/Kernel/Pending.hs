-- | Deal with pending transactions
module Cardano.Wallet.Kernel.Pending (
    newPending
  , newForeign
  , cancelPending
  , NewPendingError
  ) where

import           Universum hiding (State)

import qualified Data.List.NonEmpty as NE

import           Control.Concurrent.MVar (modifyMVar_)

import           Data.Acid.Advanced (update')

import           Pos.Core.Txp (Tx (..), TxAux (..), TxOut (..))
import           Pos.Crypto (EncryptedSecretKey)

import           Cardano.Wallet.Kernel.DB.AcidState (CancelPending (..),
                     NewForeign (..), NewForeignError (..), NewPending (..),
                     NewPendingError (..))
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.HdWallet.Create(initHdAddress)
import           Cardano.Wallet.Kernel.DB.InDb
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta, putTxMeta)
import           Cardano.Wallet.Kernel.Internal
import           Cardano.Wallet.Kernel.PrefilterTx (filterOurs)
import           Cardano.Wallet.Kernel.Read (getWalletCredentials)
import           Cardano.Wallet.Kernel.Submission (Cancelled, addPending)
import           Cardano.Wallet.Kernel.Types (WalletId (..))

import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)

{-------------------------------------------------------------------------------
  Submit pending transactions
-------------------------------------------------------------------------------}

-- | Submit a new pending transaction
--
-- If the pending transaction is successfully added to the wallet state, the
-- submission layer is notified accordingly.
--
-- NOTE: we select "our" output addresses from the transaction and pass it along to the data layer
newPending :: ActiveWallet
           -> HdAccountId
           -> TxAux
           -> Maybe TxMeta
           -> IO (Either NewPendingError ())
newPending w accountId tx mbMeta = do
    newTx w accountId tx mbMeta $ \ourAddrs ->
        update' ((walletPassive w) ^. wallets) $ NewPending accountId (InDb tx) ourAddrs

-- | Submit new foreign transaction
--
-- A foreign transaction is a transaction that transfers funds from /another/
-- wallet to this one.
newForeign :: ActiveWallet
           -> HdAccountId
           -> TxAux
           -> TxMeta
           -> IO (Either NewForeignError ())
newForeign w accountId tx meta = do
    newTx w accountId tx (Just meta) $ \ourAddrs ->
        update' ((walletPassive w) ^. wallets) $ NewForeign accountId (InDb tx) ourAddrs

-- | Submit a new transaction
--
-- Will fail if the HdAccountId does not exist or if some inputs of the
-- new transaction are not available for spending.
--
-- If the transaction is successfully added to the wallet state, transaction metadata
-- is persisted and the submission layer is notified accordingly.
--
-- NOTE: we select "our" output addresses from the transaction and pass it along to the data layer
newTx :: forall e. ActiveWallet
      -> HdAccountId
      -> TxAux
      -> Maybe TxMeta
      -> ([HdAddress] -> IO (Either e ())) -- ^ the update to run, takes ourAddrs as arg
      -> IO (Either e ())
newTx ActiveWallet{..} accountId tx mbMeta upd = do
    -- run the update
    allOurs' <- allOurs <$> getWalletCredentials walletPassive
    res <- upd allOurs'
    case res of
        Left e   -> return (Left e)
        Right () -> do
            -- process transaction on success
            putTxMeta' mbMeta
            submitTx accountId tx
            return (Right ())
    where
        addrs = NE.toList $ map txOutAddress (_txOutputs . taTx $ tx)
        wid   = WalletIdHdRnd (accountId ^. hdAccountIdParent)

        -- | NOTE: we recognise addresses in the transaction outputs that belong to _all_ wallets,
        --  not only for the wallet to which this transaction is being submitted
        allOurs :: [(WalletId, EncryptedSecretKey)] -> [HdAddress]
        allOurs = concatMap (ourAddrs . snd)

        ourAddrs :: EncryptedSecretKey -> [HdAddress]
        ourAddrs esk' =
            map f $ filterOurs wKey identity addrs
            where
                f (address,addressId) = initHdAddress addressId (InDb address)
                wKey = (wid, eskToWalletDecrCredentials esk')

        putTxMeta' :: Maybe TxMeta -> IO ()
        putTxMeta' mbMeta' =
            case mbMeta' of
                Just meta -> putTxMeta (walletPassive ^. walletMeta) meta
                Nothing   -> pure ()

        submitTx :: HdAccountId -> TxAux -> IO ()
        submitTx accountId' tx' = do
            modifyMVar_ walletSubmission (return . addPending accountId' (Pending.singleton tx'))

-- | Cancel a pending transaction
--
-- NOTE: This gets called in response to events /from/ the wallet submission
-- layer, so we shouldn't be notifying the submission in return here.
--
-- This removes the transaction from either pending or foreign.
cancelPending :: PassiveWallet -> Cancelled -> IO ()
cancelPending passiveWallet cancelled =
    update' (passiveWallet ^. wallets) $ CancelPending (fmap InDb cancelled)
