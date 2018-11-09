{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
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
import           Control.Lens (lazy, _Just)
import           Control.Monad.Except (throwError)
import           Data.Acid.Advanced (update')
import           Data.List (scanl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Genesis (Config (..))
import           Pos.Chain.Txp (TxId)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.DB.Block (getBlund)
import           Pos.Util.Log (Severity (..))

import           Cardano.Wallet.Kernel.DB.AcidState (ApplyBlock (..),
                     ObservableRollbackUseInTestsOnly (..), SwitchToFork (..),
                     SwitchToForkInternalError (..))
import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, rbContext)
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import           Cardano.Wallet.Kernel.DB.Spec.Update (ApplyBlockFailed (..))
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..),
                     prefilterBlock)
import           Cardano.Wallet.Kernel.Read (foreignPendingByAccount,
                     getWalletCredentials, getWalletSnapshot)
import           Cardano.Wallet.Kernel.Restore
import qualified Cardano.Wallet.Kernel.Submission as Submission
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util.NonEmptyMap (NonEmptyMap)
import qualified Cardano.Wallet.Kernel.Util.NonEmptyMap as NEM
import           Cardano.Wallet.WalletLayer.Kernel.Wallets
                     (blundToResolvedBlock)

{-------------------------------------------------------------------------------
  Passive Wallet API implementation
-------------------------------------------------------------------------------}

type PrefilterResult = ((BlockContext, Map HdAccountId PrefilteredBlock), [TxMeta])

-- | Prefilter the block for each account. Returns 'Nothing' if the prefiltering
-- is irrelevant for this node, i.e. there are no user wallets stored, so we
-- can avoid doing work (i.e. writing into the acid-state DB log) by skipping
-- such block application.
--
-- TODO: Improve performance (CBR-379)
prefilterBlocks :: PassiveWallet
                -> [ResolvedBlock]
                -> IO (Maybe [PrefilterResult])
prefilterBlocks pw bs = do
    let nm = makeNetworkMagic (pw ^. walletProtocolMagic)
    res <- getWalletCredentials pw
    foreignPendings <- foreignPendingByAccount <$> getWalletSnapshot pw
    return $ case res of
         [] -> Nothing
         xs -> Just $ map (\b -> first (b ^. rbContext,) $ prefilterBlock nm foreignPendings b xs) bs

data BackfillFailed
    = SuccessorChanged BlockContext (Maybe BlockContext)
      -- ^ Although we already checked that it should fit, the block we're trying
      -- to apply does not fit onto the previous block.
    | CouldNotReachCheckpoint BlockContext
      -- ^ While trying to backfill missing blocks, we found that the last known
      -- block was not an ancestor of the block to apply.
    | CouldNotFindBlockForHeader HeaderHash
      -- ^ While trying to backfill missing blocks, we got a header that did not
      -- correspond to a known block.
    | NotAMainBlock HeaderHash
      -- ^ While trying to backfill missing blocks, we got a header that did not
      -- correspond to a main block.

deriveSafeCopy 1 'base ''BackfillFailed

instance Buildable BackfillFailed where
    build (SuccessorChanged context checkpoint) = bprint
        ("SuccessorChanged "
        % "{ context:    " % build
        % ", checkpoint: " % build
        % " }"
        )
        context
        checkpoint
    build (CouldNotReachCheckpoint context) = bprint
        ("CouldNotReachCheckpoint "
        % "{ context: " % build
        % " }"
        )
        context
    build (CouldNotFindBlockForHeader hh) = bprint
        ("CouldNotFindBlockForHeader "
        % "{ header hash: " % build
        % " }"
        )
        hh
    build (NotAMainBlock hh) = bprint
        ("NotAMainBlock "
        % "{ header hash: " % build
        % " }"
        )
        hh

data ApplyBlockErrorCase
    = AccountIsBehindBlock (OldestFirst [] ResolvedBlock)
    | AccountIsAheadOfBlock
    | AccountIsOnWrongFork

-- | Notify all the wallets in the PassiveWallet of a new block
--
-- NOTE: Multiple concurrent or parallel calls to 'applyBlock' are not allowed.
-- Without this constraint, two concurrent backfill operations could conflict
-- with each other and cause both to fail.
-- The serialization of calls to 'applyBlock' is handled by the wallet worker,
-- which should carry the sole responsibility for applying blocks to a wallet.
{-# ANN applyBlock ("HLint: ignore Use forM_" :: Text) #-}
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO ()
applyBlock pw@PassiveWallet{..} b = do
    k <- Node.getSecurityParameter _walletNode
    runExceptT (applyOneBlock k Nothing b) >>= either (handleApplyBlockErrors k) pure
  where
      handleApplyBlockErrors :: Node.SecurityParameter
                             -> NonEmptyMap HdAccountId ApplyBlockFailed
                             -> IO ()
      handleApplyBlockErrors k errs = do
          -- If we could not apply this block to all accounts in all wallets, there are
          -- three things that could have gone wrong:
          --   1. An account has fallen behind the node and is missing blocks.
          --      In this case, the account's tip is an ancestor of the block to
          --      apply, and we should try to find and apply each of the missing blocks
          --      so that the account catches up to the node.
          --   2. An account's checkpoint is actually *ahead* of the given block.
          --      This can happen if a restoration begins after the node sees a block,
          --      but before the wallets have applied it. In this case, do nothing. We'll
          --      eventually process this block as part of the history restoration.
          --   3. An account's checkpoint is incomparable with the wallet worker's tip.
          --      This could happen because the account in on a different fork. In this
          --      case, start a restoration on the account's wallet.
          (toRestore, toApply) <- fmap (partitionEithers . catMaybes) $
                   forM (Map.toList $ NEM.toMap errs) $ \(acctId, failure) ->
                       classifyFailure failure <&> \case
                           AccountIsAheadOfBlock       -> Nothing
                           AccountIsBehindBlock blocks ->
                             Just (Right (acctId, blocks))
                           AccountIsOnWrongFork        ->
                             Just (Left (acctId ^. hdAccountIdParent))

          -- Start restoring each wallet that was incomparable to this block.
          for_ (Set.fromList toRestore) $ \rootId -> do
              _walletLogMessage Warning
                                ("applyBlock: block was incomparable to wallet checkpoint, "
                                 <> "restoring " <> show rootId)
              restoreKnownWallet pw rootId

          case toApply of
              []       -> return () -- nothing to do!
              (bk:bks) -> backfilling k $ gatherAcctsPerBlock (bk :| bks)

      -- Beginning with the oldest missing block, update each lagging account.
      backfilling :: Node.SecurityParameter
                  -> OldestFirst [] (ResolvedBlock, Set HdAccountId)
                  -> IO ()
      backfilling k acctsPerBlock = do
          _walletLogMessage Warning $ "Wallet is behind node by "
                                    <> (sformat build (length acctsPerBlock))
                                    <> " blocks - begin backfilling..."

          let applyOne (block, toAccts) = runExceptT $ applyOneBlock k (NE.nonEmpty (Set.toList toAccts)) block
          failures <- mapM applyOne (getOldestFirst acctsPerBlock)

          case NEM.fromMap . Map.unions . map NEM.toMap . lefts $ failures of
              Nothing ->
                  _walletLogMessage Info "Wallet has caught up with node"
              Just moreErrs -> do
                  _walletLogMessage Warning $ "More failures during backfilling - trying to handle them now: "
                                              <> (sformat build (Map.elems . NEM.toMap $ moreErrs))
                  handleApplyBlockErrors k moreErrs -- Try again, better luck next time.

      -- Try to apply a single block, failing if it does not fit onto the most recent checkpoint.
      applyOneBlock :: Node.SecurityParameter
                    -> Maybe (NE.NonEmpty HdAccountId)
                    -> ResolvedBlock
                    -> ExceptT (NonEmptyMap HdAccountId ApplyBlockFailed) IO ()
      applyOneBlock k accts b' = ExceptT $ do
          prefilterBlocks pw [b'] >>= \case
              -- The block is not relevant as there are no user wallets stored
              -- in the DB, so don't bother writing into the acid-state transaction log.
              Nothing -> return $ Right ()
              Just [((ctxt, blocksByAccount), metas)] -> do
                  -- apply block to all Accounts in all Wallets
                  mConfirmed <- update' _wallets $ ApplyBlock k ctxt accts blocksByAccount
                  case mConfirmed of
                      Left  errs      -> return (Left errs)
                      Right confirmed -> do
                          modifyMVar_ _walletSubmission (return . Submission.remPending confirmed)
                          mapM_ (putTxMeta _walletMeta) metas
                          return $ Right ()
              Just _ -> error $ "applyOneBlock: the impossible happened, "
                             <> "prefilterBlocks returned "
                             <> "a different number of elements than the input ones."

      -- Determine if a failure in 'ApplyBlock' was due to the account being ahead, behind,
      -- or incomparable with the provided block.
      classifyFailure :: ApplyBlockFailed -> IO ApplyBlockErrorCase
      classifyFailure (ApplyBlockNotSuccessor curCtx cpCtx) = do
          result <- runExceptT (findMissing (Just curCtx) cpCtx [])
          case result of
              Right blocks -> return (AccountIsBehindBlock blocks)
              Left (CouldNotReachCheckpoint _)    ->
                  -- Figure out if the checkpoint is incomparable, or from the future.
                  runExceptT (findMissing cpCtx (Just curCtx) []) <&> \case
                      Right _ -> AccountIsAheadOfBlock
                      Left  _ -> AccountIsOnWrongFork
              Left (CouldNotFindBlockForHeader _) -> return AccountIsOnWrongFork
              Left (NotAMainBlock _)              -> return AccountIsOnWrongFork
              Left (SuccessorChanged _ _)         -> return AccountIsOnWrongFork

      -- Find all blocks that were missing between the given block and the wallet's most recent
      -- checkpoint. 'Nothing' is used to represent the genesis block.
      findMissing :: Maybe BlockContext
                  -> Maybe BlockContext
                  -> [ResolvedBlock]
                  -> ExceptT BackfillFailed IO (OldestFirst [] ResolvedBlock)
      findMissing Nothing    Nothing   !acc = return (OldestFirst acc)
      findMissing Nothing    (Just cp) _acc = throwError (CouldNotReachCheckpoint cp)
      findMissing (Just cur) tgt       !acc =
        if (Just (cur ^. bcHash)) == (tgt ^? _Just . bcHash) then
            return (OldestFirst acc)
        else do
            rb   <- hashToBlock (cur ^. bcHash . fromDb)
            prev <- traverse hashToBlock (rb ^? rbContext . bcPrevMain . lazy . _Just . fromDb)
            findMissing (prev ^? _Just . rbContext) tgt (rb : acc)

      -- Find and resolve the block with a given hash.
      hashToBlock :: HeaderHash -> ExceptT BackfillFailed IO ResolvedBlock
      hashToBlock hh = ExceptT $ do
          gh <- liftIO (configGenesisHash <$> Node.getCoreConfig (pw ^. walletNode))
          Node.withNodeState (pw ^. walletNode) (\_lock -> getBlund gh hh) >>= \case
              Nothing    -> return $ Left (CouldNotFindBlockForHeader hh)
              Just blund ->
                  blundToResolvedBlock (pw ^. walletNode) blund <&> \case
                      Nothing -> Left  (NotAMainBlock hh)
                      Just rb -> Right rb

      -- Compute the list of blocks that must be applied, along with the accounts that
      -- should be updated for each block.
      -- PRECONDITION: Each list of blocks should eminate from the same "newest" block.
      gatherAcctsPerBlock :: NonEmpty (HdAccountId, OldestFirst [] ResolvedBlock)
                          -> OldestFirst [] (ResolvedBlock, Set HdAccountId)
      gatherAcctsPerBlock a2bs =
        let firstAppearedIn :: Map Int (Set HdAccountId)
            firstAppearedIn = Map.fromListWith Set.union
                              $ map (\(a, bs) -> (longestLength - length bs, Set.singleton a))
                              $ NE.toList a2bs

            -- The longest sequence of blocks; due to the precondition on gatherAcctsPerBlock,
            -- every sequence of blocks appearing in a2bs is a suffix of this sequence.
            longest :: OldestFirst [] ResolvedBlock
            longest = neMaximumBy (comparing length) $ map snd a2bs

            longestLength :: Int
            longestLength = length longest

            neMaximumBy :: (a -> a -> Ordering) -> NonEmpty a -> a
            neMaximumBy cmp = maximumBy cmp . NE.toList

            -- The accounts to update for each block in 'longest'.
            updateSets :: [Set HdAccountId]
            updateSets = drop 1 -- we're not interested in the first empty set returned by scanl'
                         $ scanl' Set.union
                                  Set.empty
                                  (map (\n -> Map.findWithDefault Set.empty n firstAppearedIn) [0..])

        in OldestFirst $ zip (getOldestFirst longest) updateSets

-- | Switch to a new fork
--
-- NOTE: The Ouroboros protocol says that this is only valid if the number of
-- resolved blocks exceeds the length of blocks to roll back.
switchToFork :: PassiveWallet
             -> Maybe HeaderHash -- ^ Roll back until we meet this hash.
             -> [ResolvedBlock] -- ^ Blocks in the new fork
             -> IO ()
switchToFork pw@PassiveWallet{..} oldest bs = do
    k <- Node.getSecurityParameter _walletNode

    blocksAndMeta <- prefilterBlocks pw bs
    case blocksAndMeta of
         -- We skip the switchToFork completely, as no wallet is configured
         -- in this node.
         Nothing -> return ()
         Just xs -> do
             let (blockssByAccount, metas) = unzip xs

             changes <- trySwitchingToFork k blockssByAccount

             mapM_ (putTxMeta _walletMeta) $ concat metas
             modifyMVar_ _walletSubmission $
               return . Submission.addPendings (fst <$> changes)
                      . Submission.remPending  (snd <$> changes)
  where

    trySwitchingToFork :: Node.SecurityParameter
                       -> [(BlockContext, Map HdAccountId PrefilteredBlock)]
                       -> IO (Map HdAccountId (Pending, Set TxId))
    trySwitchingToFork k blockssByAccount = do
        -- Find any new restorations that we didn't know about.
        restorationInfo <- currentRestorations pw
        let restorations  = Map.elems   restorationInfo
            restoringWals = Set.fromList $
              Map.keys restorationInfo <&> \case
                WalletIdHdRnd rootId -> rootId
        -- Stop the restorations.
        mapM_ cancelRestoration restorations
        -- Switch to the fork, retrying if another restoration begins in the meantime.
        update' _wallets (SwitchToFork k oldest blockssByAccount restoringWals) >>= \case
            Left badAccts -> do
                -- Some wallets need to enter restoration before we can continue.
                let badWallets = Set.fromList $ map _hdAccountIdParent badAccts
                for_ (Set.toList badWallets) $ \rootId -> do
                    _walletLogMessage Warning
                      ("switchToFork: wallet " <> show rootId <> " must enter restoration.")
                    restoreKnownWallet pw rootId

                -- Now that the problematic wallets are in restoration, try again.
                trySwitchingToFork k blockssByAccount

            Right changes -> do
                -- Restart the restorations, and return the changes.
                mapM_ restartRestoration restorations
                return changes

-- | Observable rollback
--
-- Only used for tests. See 'switchToFork'.
-- TODO(kde): Do we want tests to deal with metadata?
observableRollbackUseInTestsOnly :: PassiveWallet
                                 -> IO (Either SwitchToForkInternalError ())
observableRollbackUseInTestsOnly PassiveWallet{..} = do
    res <- update' _wallets $ ObservableRollbackUseInTestsOnly
    case res of
      Left err           -> return $ Left err
      Right reintroduced -> do modifyMVar_ _walletSubmission $
                                 return . Submission.addPendings reintroduced
                               return $ Right ()
