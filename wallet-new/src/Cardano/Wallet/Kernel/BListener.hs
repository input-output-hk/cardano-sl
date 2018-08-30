{-# LANGUAGE LambdaCase #-}
-- | React to BListener events
module Cardano.Wallet.Kernel.BListener (
    -- * Respond to block chain events
    applyBlock
  , switchToFork
    -- * Testing
  , observableRollbackUseInTestsOnly
  ) where

import           Universum hiding (State)

import           Control.Concurrent.MVar (modifyMVar_)
import           Control.Lens (_Just)
import           Control.Monad.Except (throwError)
import           Data.Acid.Advanced (update')

import           Pos.Chain.Block (HeaderHash)
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.DB.Block (getBlund)

import           Cardano.Wallet.Kernel.DB.AcidState (ApplyBlock (..),
                     ObservableRollbackUseInTestsOnly (..), SwitchToFork (..),
                     SwitchToForkError (..))
import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, rbContext)
import           Cardano.Wallet.Kernel.DB.Spec.Update (ApplyBlockFailed (..))
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..),
                     prefilterBlock)
import           Cardano.Wallet.Kernel.Read (getWalletCredentials)
import qualified Cardano.Wallet.Kernel.Submission as Submission
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.WalletLayer.Kernel.Wallets
                     (blundToResolvedBlock)

{-------------------------------------------------------------------------------
  Passive Wallet API implementation
-------------------------------------------------------------------------------}

-- | Prefilter the block for each account.
--
-- TODO: Improve performance (CBR-379)
prefilterBlock' :: PassiveWallet
                -> ResolvedBlock
                -> IO ((BlockContext, Map HdAccountId PrefilteredBlock), [TxMeta])
prefilterBlock' pw b = do
    aux <$> getWalletCredentials pw
  where
    aux :: [(WalletId, EncryptedSecretKey)]
        -> ((BlockContext, Map HdAccountId PrefilteredBlock), [TxMeta])
    aux ws =
      let (conMap, conMeta) = mconcat $ map (uncurry (prefilterBlock b)) ws
      in ((b ^. rbContext, conMap), conMeta)

-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO (Either ApplyBlockFailed ())
applyBlock pw@PassiveWallet{..} b = do
    k <- Node.getSecurityParameter _walletNode
    ((ctxt, blocksByAccount), metas) <- prefilterBlock' pw b
    -- apply block to all Accounts in all Wallets
    mConfirmed <- update' _wallets $ ApplyBlock k ctxt blocksByAccount
    case mConfirmed of
      Left  (ApplyBlockNotSuccessor curCtx cpCtx) -> runExceptT (backfill (Just curCtx) cpCtx)
      Left err -> return $ Left err
      Right confirmed -> do
        modifyMVar_ _walletSubmission $ return . Submission.remPending confirmed
        mapM_ (putTxMeta _walletMeta) metas
        return $ Right ()

  where
      -- Backfill blocks that were missing between the given block and the wallet's most recent
      -- checkpoint. 'backfill src tgt' will apply all blocks in order, starting from the
      -- successor of tgt until src is reached. 'Nothing' is used to represent the genesis block.
      backfill :: Maybe BlockContext -> Maybe BlockContext -> ExceptT ApplyBlockFailed IO ()
      backfill Nothing Nothing   = return ()
      backfill Nothing (Just cp) = throwError (CouldNotReachCheckpoint cp)
      backfill (Just cur) tgt    =
        if (Just (cur ^. bcHash)) == (tgt ^? _Just . bcHash) then
            return ()
        else do
            rb   <- hashToBlock (cur ^. bcHash . fromDb)
            prev <- traverse hashToBlock (rb ^? rbContext . bcPrevMain . _Just . fromDb)
            backfill (prev ^? _Just . rbContext) tgt
            ExceptT (applyBlock pw rb)

      -- Find and resolve the block with a given hash.
      hashToBlock :: HeaderHash -> ExceptT ApplyBlockFailed IO ResolvedBlock
      hashToBlock hh = ExceptT $
          Node.withNodeState (pw ^. walletNode) (\_lock -> getBlund hh) >>= \case
              Nothing    -> return $ Left (CouldNotFindBlockForHeader hh)
              Just blund ->
                  blundToResolvedBlock (pw ^. walletNode) blund <&> \case
                      Nothing -> Left  (NotAMainBlock hh)
                      Just rb -> Right rb

-- | Switch to a new fork
--
-- NOTE: The Ouroboros protocol says that this is only valid if the number of
-- resolved blocks exceeds the length of blocks to roll back.
switchToFork :: PassiveWallet
             -> Int             -- ^ Number of blocks to roll back
             -> [ResolvedBlock] -- ^ Blocks in the new fork
             -> IO (Either SwitchToForkError ())
switchToFork pw@PassiveWallet{..} n bs = do
    k <- Node.getSecurityParameter _walletNode
    blocksAndMeta <- mapM (prefilterBlock' pw) bs
    let (blockssByAccount, metas) = unzip blocksAndMeta
    res <- update' _wallets $ SwitchToFork k n blockssByAccount
    case res of
      Left  err     -> return $ Left err
      Right changes -> do mapM_ (putTxMeta _walletMeta) $ concat metas
                          modifyMVar_ _walletSubmission $
                            return . Submission.addPendings (fst <$> changes)
                          modifyMVar_ _walletSubmission $
                            return . Submission.remPending (snd <$> changes)
                          return $ Right ()

-- | Observable rollback
--
-- Only used for tests. See 'switchToFork'.
-- TODO(kde): Do we want tests to deal with metadata?
observableRollbackUseInTestsOnly :: PassiveWallet
                                 -> IO (Either SwitchToForkError ())
observableRollbackUseInTestsOnly PassiveWallet{..} = do
    res <- update' _wallets $ ObservableRollbackUseInTestsOnly
    case res of
      Left err           -> return $ Left err
      Right reintroduced -> do modifyMVar_ _walletSubmission $
                                 return . Submission.addPendings reintroduced
                               return $ Right ()
