-- | Deal with pending transactions
module Cardano.Wallet.Kernel.Pending (
    newPending
  , newForeign
  , cancelPending
  , NewPendingError
  , PartialTxMeta
  ) where

import           Universum hiding (State)

import qualified Data.List.NonEmpty as NE

import           Control.Concurrent.MVar (modifyMVar_)

import           Data.Acid.Advanced (update')

import           Pos.Chain.Txp (Tx (..), TxAux (..), TxOut (..))
import           Pos.Core (Coin (..))
import           Pos.Crypto (EncryptedSecretKey)

import           Cardano.Wallet.Kernel.DB.AcidState (CancelPending (..),
                     NewForeign (..), NewForeignError (..), NewPending (..),
                     NewPendingError (..))
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdAddress)
import           Cardano.Wallet.Kernel.DB.InDb
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta, putTxMeta)
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentialsKey (..),
                     keyToWalletDecrCredentials)
import           Cardano.Wallet.Kernel.Internal
import           Cardano.Wallet.Kernel.PrefilterTx (filterOurs)
import           Cardano.Wallet.Kernel.Read (getWalletCredentials)
import           Cardano.Wallet.Kernel.Submission (Cancelled, addPending)
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core

{-------------------------------------------------------------------------------
  Submit pending transactions
-------------------------------------------------------------------------------}

-- | When we create a new Transaction, we don`t yet know which outputs belong to us
-- (it may not be just the change addresses change we create, but also addresses the user specifies).
-- This check happenes in @newTx@. Until then we move around this partial TxMetadata.
-- @Bool@ indicates if all outputs are ours and @Coin@ the sum of the coin of our outputs.
type PartialTxMeta = Bool -> Coin -> TxMeta

-- | Submit a new pending transaction
--
-- If the pending transaction is successfully added to the wallet state, the
-- submission layer is notified accordingly.
--
-- NOTE: we select "our" output addresses from the transaction and pass it along to the data layer
newPending :: ActiveWallet
           -> HdAccountId
           -> TxAux
           -> PartialTxMeta
           -> IO (Either NewPendingError TxMeta)
newPending w accountId tx partialMeta = do
    newTx w accountId tx partialMeta $ \ourAddrs ->
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
    map void <$> newTx w accountId tx (\_ _ ->  meta) $ \ourAddrs ->
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
      -> PartialTxMeta
      -> ([HdAddress] -> IO (Either e ())) -- ^ the update to run, takes ourAddrs as arg
      -> IO (Either e TxMeta)
newTx ActiveWallet{..} accountId tx partialMeta upd = do
    -- run the update
    allCredentials <- getWalletCredentials walletPassive
    let allOurAddresses = fst <$> allOurs allCredentials
    res <- upd $ allOurAddresses
    case res of
        Left e   -> return (Left e)
        Right () -> do
            -- process transaction on success
            -- myCredentials should be a list with a single element.
            let myCredentials = filter (\(WalletIdHdRnd hdRoot, _) -> accountId ^. hdAccountIdParent == hdRoot) allCredentials
                ourOutputCoins = snd <$> allOurs myCredentials
                gainedOutputCoins = sumCoinsUnsafe ourOutputCoins
                allOutsOurs = length ourOutputCoins == length txOut
                txMeta = partialMeta allOutsOurs gainedOutputCoins
            putTxMeta (walletPassive ^. walletMeta) txMeta
            submitTx
            return (Right txMeta)
    where
        (txOut :: [TxOut]) = NE.toList $ (_txOutputs . taTx $ tx)

        -- | NOTE: we recognise addresses in the transaction outputs that belong to _all_ wallets,
        --  not only for the wallet to which this transaction is being submitted
        allOurs :: [(WalletId, EncryptedSecretKey)] -> [(HdAddress,Coin)]
        allOurs = concatMap ourAddrs

        ourAddrs :: (WalletId, EncryptedSecretKey) -> [(HdAddress,Coin)]
        ourAddrs (wid, esk) =
            map f $ filterOurs wKey txOutAddress txOut
            where
                f (txOut',addressId) = (initHdAddress addressId (txOutAddress txOut'), txOutValue txOut')
                wKey = (wid, keyToWalletDecrCredentials $ KeyForRegular esk)

        submitTx :: IO ()
        submitTx = modifyMVar_ (walletPassive ^. walletSubmission) $
                    return . addPending accountId (Pending.singleton tx)

-- | Cancel a pending transaction
--
-- NOTE: This gets called in response to events /from/ the wallet submission
-- layer, so we shouldn't be notifying the submission in return here.
--
-- This removes the transaction from either pending or foreign.
cancelPending :: PassiveWallet -> Cancelled -> IO ()
cancelPending passiveWallet cancelled =
    update' (passiveWallet ^. wallets) $ CancelPending (fmap InDb cancelled)
