-- | React to BListener events
module Cardano.Wallet.Kernel.BListener (
    -- * Respond to block chain events
    applyBlock
  , applyBlocks
  , switchToFork
    -- * Testing
  , observableRollbackUseInTestsOnly
  ) where

import           Universum hiding (State)

import           Data.Acid.Advanced (update')
import qualified Data.Map.Strict as Map

import           Pos.Core (SlotId)
import           Pos.Core.Chrono (OldestFirst)
import           Pos.Crypto (EncryptedSecretKey)

import           Cardano.Wallet.Kernel.DB.AcidState (ApplyBlock (..),
                     ObservableRollbackUseInTestsOnly (..),
                     RollbackDuringRestoration, SwitchToFork (..))
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, rbSlotId)
import           Cardano.Wallet.Kernel.Internal
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..),
                     prefilterBlock)
import           Cardano.Wallet.Kernel.Read (getWalletCredentials)
import           Cardano.Wallet.Kernel.Types (WalletId (..))

{-------------------------------------------------------------------------------
  Passive Wallet API implementation
-------------------------------------------------------------------------------}

-- | Prefilter the block for each account.
--
-- TODO: Improve performance (CBR-379)
prefilterBlock' :: PassiveWallet
                -> ResolvedBlock
                -> IO (SlotId, Map HdAccountId PrefilteredBlock)
prefilterBlock' pw b = do
    aux <$> getWalletCredentials pw
  where
    aux :: [(WalletId, EncryptedSecretKey)]
        -> (SlotId, Map HdAccountId PrefilteredBlock)
    aux ws = (
        b ^. rbSlotId
      , Map.unions $ map (uncurry (prefilterBlock b)) ws
      )

-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO ()
applyBlock pw@PassiveWallet{..} b
    = do
        (slotId, blocksByAccount) <- prefilterBlock' pw b
        -- apply block to all Accounts in all Wallets
        update' _wallets $ ApplyBlock (InDb slotId) blocksByAccount

-- | Apply multiple blocks, one at a time, to all wallets in the PassiveWallet
--
--   TODO(@matt-noonan) this will be the responsibility of the worker thread (as part of CBR-243: Wallet restoration)
applyBlocks :: PassiveWallet
            -> OldestFirst [] ResolvedBlock
            -> IO ()
applyBlocks = mapM_ . applyBlock

-- | Switch to a new fork
--
-- NOTE: The Ouroboros protocol says that this is only valid if the number of
-- resolved blocks exceeds the length of blocks to roll back.
switchToFork :: PassiveWallet
             -> Int             -- ^ Number of blocks to roll back
             -> [ResolvedBlock] -- ^ Blocks in the new fork
             -> IO (Either RollbackDuringRestoration ())
switchToFork pw@PassiveWallet{..} n bs = do
    blockssByAccount <- mapM (prefilterBlock' pw) bs
    update' _wallets $ SwitchToFork n blockssByAccount

-- | Observable rollback
--
-- Only used for tests. See 'switchToFork'.
observableRollbackUseInTestsOnly :: PassiveWallet
                                 -> IO (Either RollbackDuringRestoration ())
observableRollbackUseInTestsOnly PassiveWallet{..} =
    update' _wallets $ ObservableRollbackUseInTestsOnly
