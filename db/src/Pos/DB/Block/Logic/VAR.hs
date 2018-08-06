-- | Center of where block verification/application/rollback happens.
-- Unifies VAR for all components (slog, ssc, txp, dlg, us).

module Pos.DB.Block.Logic.VAR
       ( verifyBlocksPrefix

       , BlockLrcMode
       , verifyAndApplyBlocks
       , applyWithRollback

       -- * Exported for tests
       , applyBlocks
       , rollbackBlocks
       ) where

import           Universum

import           Control.Exception.Safe (bracketOnError)
import           Control.Lens (_Wrapped)
import           Control.Monad.Except (ExceptT (ExceptT),
                     MonadError (throwError), runExceptT, withExceptT)
import qualified Data.List.NonEmpty as NE
import           Formatting (sformat, shown, (%))
import           UnliftIO (UnliftIO (..), askUnliftIO)

import           Pos.Chain.Block (ApplyBlocksException (..), Blund,
                     RollbackException (..), Undo (..),
                     VerifyBlocksException (..))
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Chain.Update (PollModifier)
import           Pos.Core (epochIndexL)
import           Pos.Core.Block (Block, HeaderHash, headerHashG, prevBlockL)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..),
                     toNewestFirst, toOldestFirst)
import           Pos.Core.Reporting (HasMisbehaviorMetrics)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.Block.Logic.Internal (BypassSecurityCheck (..),
                     MonadBlockApply, MonadBlockVerify,
                     MonadMempoolNormalization, applyBlocksUnsafe,
                     normalizeMempool, rollbackBlocksUnsafe, toSscBlock,
                     toTxpBlock, toUpdateBlock)
import           Pos.DB.Block.Logic.Types (VerifyBlocksContext (..),
                     getVerifyBlocksContext)
import           Pos.DB.Block.Lrc (LrcModeFull, lrcSingleShot)
import           Pos.DB.Block.Slog.Logic (ShouldCallBListener (..),
                     mustDataBeKnown, slogVerifyBlocks)
import           Pos.DB.Delegation (dlgVerifyBlocks)
import qualified Pos.DB.GState.Common as GS (getTip)
import           Pos.DB.Ssc (sscVerifyBlocks)
import           Pos.DB.Txp.Settings
                     (TxpGlobalSettings (TxpGlobalSettings, tgsVerifyBlocks))
import           Pos.DB.Update (getAdoptedBVFull, usVerifyBlocks)
import           Pos.Util (neZipWith4, spanSafe, _neHead)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logDebug,
                     natTrace)
import           Pos.Util.Util (HasLens (..))

-- -- CHECK: @verifyBlocksLogic
-- -- #txVerifyBlocks
-- -- #sscVerifyBlocks
-- -- #dlgVerifyBlocks
-- -- #usVerifyBlocks
-- | Verify new blocks. If parent of the first block is not our tip,
-- verification fails. All blocks must be from the same epoch.  This
-- function checks literally __everything__ from blocks, including
-- header, body, extra data, etc.
--
-- LRC must be already performed for the epoch from which blocks are.
--
-- Algorithm:
-- 1.  Ensure that the parent of the oldest block that we want to apply
--     matches the current tip.
-- 2.  Perform verification component-wise (@slog@, @ssc@, @txp@, @dlg@, @us@).
-- 3.  Ensure that the number of undos from @txp@ and @dlg@ is the same.
-- 4.  Return all undos.
verifyBlocksPrefix
    :: forall ctx m.
       ( MonadBlockVerify ctx m
       )
    => TraceNamed m
    -> ProtocolMagic
    -> VerifyBlocksContext
    -> OldestFirst NE Block
    -> m (Either VerifyBlocksException (OldestFirst NE Undo, PollModifier))
verifyBlocksPrefix logTrace0 pm ctx blocks = do
  let logTrace = appendName "verifyBlocksPrefix" logTrace0
  runExceptT $ do
    -- This check (about tip) is here just in case, we actually check
    -- it before calling this function.
    tip <- lift GS.getTip
    when (tip /= blocks ^. _Wrapped . _neHead . prevBlockL) $
        throwError $ VerifyBlocksError "the first block isn't based on the tip"
    -- Some verifications need to know whether all data must be known.
    -- We determine it here and pass to all interested components.
    let dataMustBeKnown = mustDataBeKnown (vbcBlockVersion ctx)

    -- Run verification of each component.
    -- 'slogVerifyBlocks' uses 'Pos.Chain.Block.Pure.verifyBlocks' which does
    -- the internal consistency checks formerly done in the 'Bi' instance
    -- 'decode'.
    slogUndos <- withExceptT VerifyBlocksError $
        ExceptT $ slogVerifyBlocks pm ctx blocks
    _ <- withExceptT (VerifyBlocksError . pretty) $
        ExceptT $ sscVerifyBlocks logTrace pm (vbcBlockVersionData ctx) (map toSscBlock blocks)
    TxpGlobalSettings {..} <- view (lensOf @TxpGlobalSettings)
    txUndo <- withExceptT (VerifyBlocksError . pretty) $
        ExceptT $ tgsVerifyBlocks logTrace dataMustBeKnown $ map toTxpBlock blocks
    pskUndo <- withExceptT VerifyBlocksError $ dlgVerifyBlocks pm blocks
    un <- lift askUnliftIO
    (pModifier, usUndos) <- withExceptT (VerifyBlocksError . pretty) $
        ExceptT $ usVerifyBlocks (natTrace (unliftIO un) logTrace) pm dataMustBeKnown (vbcBlockVersion ctx) (map toUpdateBlock blocks)

    -- Eventually we do a sanity check just in case and return the result.
    when (length txUndo /= length pskUndo) $
        throwError $ VerifyBlocksError
        "Internal error of verifyBlocksPrefix: lengths of undos don't match"
    pure ( OldestFirst $ neZipWith4 Undo
               (getOldestFirst txUndo)
               (getOldestFirst pskUndo)
               (getOldestFirst usUndos)
               (getOldestFirst slogUndos)
         , pModifier)

-- | Union of constraints required by block processing and LRC.
type BlockLrcMode ctx m = (MonadBlockApply ctx m, LrcModeFull ctx m)

-- | Applies a list of blocks (not necessarily from a single epoch) if they
-- are valid. Takes one boolean flag @rollback@. On success, normalizes all
-- mempools except the delegation one and returns the header hash of the last
-- applied block (the new tip). Failure behavior depends on the @rollback@
-- flag. If it's on, all blocks applied inside this function will be rolled
-- back, so it will do effectively nothing and return @Left error@. If it's
-- off, it will try to apply as many blocks as possible. If the very first
-- block cannot be applied, it will throw an exception, otherwise it will
-- return the header hash of the new tip. It's up to the caller to log a
-- warning that partial application has occurred.
verifyAndApplyBlocks
    :: forall ctx m.
       ( BlockLrcMode ctx m
       , MonadMempoolNormalization ctx m
       , HasMisbehaviorMetrics ctx
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> VerifyBlocksContext
    -> Bool
    -> OldestFirst NE Block
    -> m (Either ApplyBlocksException (HeaderHash, NewestFirst [] Blund))
verifyAndApplyBlocks logTrace0 pm txpConfig ctx rollback blocks = runExceptT $ do
    tip <- lift GS.getTip
    let assumedTip = blocks ^. _Wrapped . _neHead . prevBlockL
    when (tip /= assumedTip) $
        throwError $ ApplyBlocksTipMismatch "verify and apply" tip assumedTip
    hh <- rollingVerifyAndApply [] (spanEpoch blocks)
    lift $ normalizeMempool logTrace' pm txpConfig
    pure hh
  where
    logTrace = appendName "verifyAndApplyBlocks" logTrace0
    logTrace' = natTrace liftIO logTrace
    logDebug' :: Text -> m ()
    logDebug' = logDebug logTrace'
    -- Spans input into @(a, b)@ where @a@ is either a single genesis
    -- block or a maximum prefix of main blocks from the same epoch.
    -- Examples (where g is for genesis and m is for main):
    -- * gmmgm -> g, mmgm
    -- * mmgm -> mm, gm
    -- * ggmmg -> g, gmmg
    spanEpoch ::
           OldestFirst NE Block
        -> (OldestFirst NE Block, OldestFirst [] Block)
    spanEpoch (OldestFirst (b@(Left _):|xs)) = (OldestFirst $ b:|[], OldestFirst xs)
    spanEpoch x                              = spanTail x
    spanTail = over _1 OldestFirst . over _2 OldestFirst .  -- wrap both results
                spanSafe ((==) `on` view epochIndexL) .      -- do work
                getOldestFirst                               -- unwrap argument
    -- Applies as many blocks from failed prefix as possible. Argument
    -- indicates if at least some progress was done so we should
    -- return tip. Fail otherwise.
    applyAMAP
        :: ApplyBlocksException
        -> OldestFirst [] Block
        -> NewestFirst [] Blund  -- an accumulator for `Blund`s
        -> Bool
        -> ExceptT ApplyBlocksException m (HeaderHash, NewestFirst [] Blund)
    applyAMAP e (OldestFirst []) _      True                   = throwError e
    applyAMAP _ (OldestFirst []) blunds False                  = (,blunds) <$> lift GS.getTip
    applyAMAP e (OldestFirst (block:xs)) blunds nothingApplied =
        lift (verifyBlocksPrefix logTrace' pm ctx (one block)) >>= \case
            Left (ApplyBlocksVerifyFailure -> e') ->
                applyAMAP e' (OldestFirst []) blunds nothingApplied
            Right (OldestFirst (undo :| []), pModifier) -> do
                lift $ applyBlocksUnsafe logTrace pm
                    (vbcBlockVersion ctx)
                    (vbcBlockVersionData ctx)
                    (ShouldCallBListener True)
                    (one (block, undo))
                    (Just pModifier)
                applyAMAP e (OldestFirst xs) (NewestFirst $ (block, undo) : getNewestFirst blunds) False
            Right _ -> error "verifyAndApplyBlocksInternal: applyAMAP: \
                             \verification of one block produced more than one undo"
    -- Rollbacks and returns an error
    failWithRollback
        :: ApplyBlocksException
        -> [NewestFirst NE Blund]
        -> ExceptT ApplyBlocksException m (HeaderHash, NewestFirst [] Blund)
    failWithRollback e toRollback = do
        lift $ logDebug' "verifyAndapply failed, rolling back"
        lift $ mapM_ (rollbackBlocks logTrace pm) toRollback
        throwError e
    -- This function tries to apply a new portion of blocks (prefix
    -- and suffix). It also has an aggregating parameter @blunds@ which is
    -- collected to rollback blocks if correspondent flag is on. The first
    -- list is packs of blunds. The head of this list is the blund that should
    -- be rolled back first. This function also tries to apply as much as
    -- possible if the @rollback@ flag is on.
    rollingVerifyAndApply
        :: [NewestFirst NE Blund]
        -> (OldestFirst NE Block, OldestFirst [] Block)
        -> ExceptT ApplyBlocksException m (HeaderHash, NewestFirst [] Blund)
    rollingVerifyAndApply blunds (prefix, suffix) = do
        let prefixHead = prefix ^. _Wrapped . _neHead
        when (isLeft prefixHead) $ do
            let epochIndex = prefixHead ^. epochIndexL
            lift $ logDebug' $ "Rolling: Calculating LRC if needed for epoch "
                       <> pretty epochIndex
            lift $ lrcSingleShot logTrace pm epochIndex
        lift $ logDebug' "Rolling: verifying"
        lift (verifyBlocksPrefix logTrace' pm ctx prefix) >>= \case
            Left (ApplyBlocksVerifyFailure -> failure)
                | rollback  -> failWithRollback failure blunds
                | otherwise -> do
                      lift $ logDebug' $ sformat ("Rolling: run applyAMAP: apply as much as possible\
                                                  \ after `verifyBlocksPrefix` failure: "%shown) failure
                      applyAMAP failure
                                   (over _Wrapped toList prefix)
                                   (NewestFirst [])
                                   (null blunds)
            Right (undos, pModifier) -> do
                let newBlunds = OldestFirst $ getOldestFirst prefix `NE.zip`
                                              getOldestFirst undos
                let blunds' = toNewestFirst newBlunds : blunds
                lift $ logDebug' "Rolling: Verification done, applying unsafe block"
                lift $ applyBlocksUnsafe logTrace pm
                    (vbcBlockVersion ctx)
                    (vbcBlockVersionData ctx)
                    (ShouldCallBListener True)
                    newBlunds
                    (Just pModifier)
                case getOldestFirst suffix of
                    [] -> (,concatNE blunds') <$> lift GS.getTip
                    (genesis:xs) -> do
                        lift $ logDebug' "Rolling: Applying done, next portion"
                        rollingVerifyAndApply blunds' $
                            spanEpoch (OldestFirst (genesis:|xs))

    concatNE :: [NewestFirst NE a] -> NewestFirst [] a
    concatNE = NewestFirst . foldMap (\(NewestFirst as) -> NE.toList as)

-- | Apply definitely valid sequence of blocks. At this point we must
-- have verified all predicates regarding block (including txp and ssc
-- data checks). We also must have taken lock on block application
-- and ensured that chain is based on our tip. Blocks will be applied
-- per-epoch, calculating lrc when needed if flag is set.
applyBlocks
    :: forall ctx m
     . ( BlockLrcMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> Bool
    -> Maybe PollModifier
    -> OldestFirst NE Blund
    -> m ()
applyBlocks logTrace pm calculateLrc pModifier blunds = do
    when (isLeft prefixHead && calculateLrc) $
        -- Hopefully this lrc check is never triggered -- because
        -- caller most definitely should have computed lrc to verify
        -- the sequence beforehand.
        lrcSingleShot logTrace pm (prefixHead ^. epochIndexL)
    (bv, bvd) <- getAdoptedBVFull
    applyBlocksUnsafe logTrace pm bv bvd (ShouldCallBListener True) prefix pModifier
    case getOldestFirst suffix of
        []           -> pass
        (genesis:xs) -> applyBlocks logTrace pm calculateLrc pModifier (OldestFirst (genesis:|xs))
  where
    prefixHead = prefix ^. _Wrapped . _neHead . _1
    (prefix, suffix) = spanEpoch blunds
    -- this version is different from one in verifyAndApply subtly,
    -- but they can be merged with some struggle.
    spanEpoch (OldestFirst (b@((Left _),_):|xs)) = (OldestFirst $ b:|[], OldestFirst xs)
    spanEpoch x                                  = spanTail x
    spanTail = over _1 OldestFirst . over _2 OldestFirst .
               spanSafe ((==) `on` view (_1 . epochIndexL)) .
               getOldestFirst

-- | Rollbacks blocks. Head must be the current tip.
rollbackBlocks
    :: MonadBlockApply ctx m
    => TraceNamed IO
    -> ProtocolMagic
    -> NewestFirst NE Blund
    -> m ()
rollbackBlocks logTrace pm blunds = do
    tip <- GS.getTip
    let firstToRollback = blunds ^. _Wrapped . _neHead . _1 . headerHashG
    when (tip /= firstToRollback) $
        throwM $ RollbackTipMismatch tip firstToRollback
    rollbackBlocksUnsafe logTrace pm (BypassSecurityCheck False) (ShouldCallBListener True) blunds

-- | Rollbacks some blocks and then applies some blocks.
applyWithRollback
    :: forall ctx m
     . ( BlockLrcMode ctx m
       , MonadMempoolNormalization ctx m
       , HasMisbehaviorMetrics ctx
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> NewestFirst NE Blund        -- ^ Blocks to rollbck
    -> OldestFirst NE Block        -- ^ Blocks to apply
    -> m (Either ApplyBlocksException HeaderHash)
applyWithRollback logTrace pm txpConfig toRollback toApply = runExceptT $ do
    tip <- lift GS.getTip
    when (tip /= newestToRollback) $
        throwError $ ApplyBlocksTipMismatch "applyWithRollback/rollback" tip newestToRollback

    let doRollback = rollbackBlocksUnsafe
            logTrace
            pm
            (BypassSecurityCheck False)
            (ShouldCallBListener True)
            toRollback
    -- We want to make sure that if we successfully perform a
    -- rollback, we will eventually apply old or new blocks.
    ExceptT $ bracketOnError doRollback (\_ -> applyBack) $ \_ -> do
        tipAfterRollback <- GS.getTip
        if tipAfterRollback /= expectedTipApply
            then onBadRollback tip
            else onGoodRollback
  where
    reApply = toOldestFirst toRollback
    applyBack :: m ()
    applyBack = applyBlocks logTrace pm False Nothing reApply
    expectedTipApply = toApply ^. _Wrapped . _neHead . prevBlockL
    newestToRollback = toRollback ^. _Wrapped . _neHead . _1 . headerHashG

    onBadRollback tip =
        applyBack $> Left (ApplyBlocksTipMismatch "applyWithRollback/apply" tip newestToRollback)

    onGoodRollback = do
        ctx <- getVerifyBlocksContext
        verifyAndApplyBlocks logTrace pm txpConfig ctx True toApply >>= \case
            Left err           -> applyBack $> Left err
            Right (tipHash, _) -> pure (Right tipHash)
