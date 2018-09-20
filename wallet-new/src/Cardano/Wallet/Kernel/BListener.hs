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
import           Control.Lens (to)
import           Data.Acid (createCheckpoint)
import           Data.Acid.Advanced (update')

import           Pos.Core (getSlotIndex, siSlotL)
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Util.Log (Severity (Info))

import           Cardano.Wallet.Kernel.DB.AcidState (ApplyBlock (..),
                     ObservableRollbackUseInTestsOnly (..), SwitchToFork (..),
                     SwitchToForkError (..))
import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, rbContext)
import           Cardano.Wallet.Kernel.DB.Spec.Update (ApplyBlockFailed)
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..),
                     prefilterBlock)
import           Cardano.Wallet.Kernel.Read (getWalletCredentials)
import qualified Cardano.Wallet.Kernel.Submission as Submission
import           Cardano.Wallet.Kernel.Types (WalletId (..))

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
      Left  err       -> return $ Left err
      Right confirmed -> do
        modifyMVar_ _walletSubmission $ return . Submission.remPending confirmed
        mapM_ (putTxMeta _walletMeta) metas
        createCheckpointIfNeeded
        return $ Right ()
  where
      -- | Interim fix, see CBR-438 and
      -- https://github.com/acid-state/acid-state/issues/103. In brief, when
      -- the note initially syncs and lots of blocks gets passed to the wallet
      -- worker, a new `ApplyBlock` acidic transaction will be committed on the
      -- transaction log but not written to disk _yet_ (that's what checkpoints
      -- are for). However, this might lead to memory leaks if such checkpointing
      -- step doesn't happen fast enough. Therefore, every time we apply a block,
      -- we decrement this counter and when it reaches 0, we enforce a new
      -- checkpoint.
      createCheckpointIfNeeded :: IO ()
      createCheckpointIfNeeded = do
          -- Look at the 'ResolvedBlock' 's 'SlotId', and assess if a new
          -- checkpoint is needed by doing @localBlockIx `modulo` someConstant@
          -- where @someConstant@ is chosen to be 1000.
          let blockSlotIx = b ^. rbContext
                               . bcSlotId
                               . fromDb
                               . siSlotL
                               . to getSlotIndex
              checkpointNeeded = (blockSlotIx - 1) `mod` 1000 == 0
          when checkpointNeeded $ do
              _walletLogMessage Info "applyBlock: making an acid-state DB checkpoint..."
              createCheckpoint _wallets

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
