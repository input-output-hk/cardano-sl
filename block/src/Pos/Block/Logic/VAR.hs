-- | Verify|apply|rollback logic.

module Pos.Block.Logic.VAR
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
import           Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT,
                                       withExceptT)
import qualified Data.List.NonEmpty as NE
import           Ether.Internal (HasLens (..))
import           System.Wlog (logDebug)

import           Pos.Block.Error (ApplyBlocksException (..), RollbackException (..),
                                  VerifyBlocksException (..))
import           Pos.Block.Logic.Internal (BypassSecurityCheck (..), MonadBlockApply,
                                           MonadBlockVerify, MonadMempoolNormalization,
                                           applyBlocksUnsafe, normalizeMempool,
                                           rollbackBlocksUnsafe, toSscBlock, toTxpBlock,
                                           toUpdateBlock)
import           Pos.Block.Slog (ShouldCallBListener (..), mustDataBeKnown, slogVerifyBlocks)
import           Pos.Block.Types (Blund, Undo (..))
import           Pos.Core (Block, HeaderHash, epochIndexL, headerHashG, prevBlockL)
import qualified Pos.DB.GState.Common as GS (getTip)
import           Pos.Delegation.Logic (dlgVerifyBlocks)
import           Pos.Lrc.Worker (LrcModeFull, lrcSingleShot)
import           Pos.Ssc (sscVerifyBlocks)
import           Pos.Txp.Settings (TxpGlobalSettings (..))
import qualified Pos.Update.DB as GS (getAdoptedBV)
import           Pos.Update.Logic (usVerifyBlocks)
import           Pos.Update.Poll (PollModifier)
import           Pos.Util (neZipWith4, spanSafe, _neHead)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..), toNewestFirst,
                                  toOldestFirst)

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
verifyBlocksPrefix
    :: forall ctx m.
       (MonadBlockVerify ctx m)
    => OldestFirst NE Block
    -> m (Either VerifyBlocksException (OldestFirst NE Undo, PollModifier))
verifyBlocksPrefix blocks = runExceptT $ do
    -- This check (about tip) is here just in case, we actually check
    -- it before calling this function.
    tip <- GS.getTip
    when (tip /= blocks ^. _Wrapped . _neHead . prevBlockL) $
        throwError $ VerifyBlocksError "the first block isn't based on the tip"
    -- Some verifications need to know whether all data must be known.
    -- We determine it here and pass to all interested components.
    adoptedBV <- GS.getAdoptedBV
    let dataMustBeKnown = mustDataBeKnown adoptedBV

    -- And then we run verification of each component.
    slogUndos <- withExceptT VerifyBlocksError $ slogVerifyBlocks blocks
    _ <- withExceptT (VerifyBlocksError . pretty) $
        ExceptT $ sscVerifyBlocks (map toSscBlock blocks)
    TxpGlobalSettings {..} <- view (lensOf @TxpGlobalSettings)
    txUndo <- withExceptT (VerifyBlocksError . pretty) $
        tgsVerifyBlocks dataMustBeKnown $ map toTxpBlock blocks
    pskUndo <- withExceptT VerifyBlocksError $ dlgVerifyBlocks blocks
    (pModifier, usUndos) <- withExceptT (VerifyBlocksError . pretty) $
        ExceptT $ usVerifyBlocks dataMustBeKnown (map toUpdateBlock blocks)

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

-- | Applies blocks if they're valid. Takes one boolean flag
-- "rollback". Returns header hash of last applied block (new tip) on
-- success. Failure behaviour depends on "rollback" flag. If it's on,
-- all blocks applied inside this function will be rollbacked, so it
-- will do effectively nothing and return 'Left error'. If it's off,
-- it will try to apply as much blocks as it's possible and return
-- header hash of new tip. It's up to caller to log warning that
-- partial application happened.
verifyAndApplyBlocks
    :: forall ctx m. (BlockLrcMode ctx m, MonadMempoolNormalization ctx m)
    => Bool -> OldestFirst NE Block -> m (Either ApplyBlocksException HeaderHash)
verifyAndApplyBlocks rollback blocks = runExceptT $ do
    tip <- GS.getTip
    let assumedTip = blocks ^. _Wrapped . _neHead . prevBlockL
    when (tip /= assumedTip) $
        throwError $ ApplyBlocksTipMismatch "verify and apply" tip assumedTip
    hh <- rollingVerifyAndApply [] (spanEpoch blocks)
    lift $ normalizeMempool
    pure hh
  where
    spanEpoch ::
           OldestFirst NE Block
        -> (OldestFirst NE Block, OldestFirst [] Block)
    spanEpoch (OldestFirst (b@(Left _):|xs)) = (OldestFirst $ b:|[], OldestFirst xs)
    spanEpoch x                              = spanTail x
    spanTail = over _1 OldestFirst . over _2 OldestFirst .  -- wrap both results
                spanSafe ((==) `on` view epochIndexL) .      -- do work
                getOldestFirst                               -- unwrap argument
    -- Applies as much blocks from failed prefix as possible. Argument
    -- indicates if at least some progress was done so we should
    -- return tip. Fail otherwise.
    applyAMAP e (OldestFirst []) True                   = throwError e
    applyAMAP _ (OldestFirst []) False                  = GS.getTip
    applyAMAP e (OldestFirst (block:xs)) nothingApplied =
        lift (verifyBlocksPrefix (one block)) >>= \case
            Left (ApplyBlocksVerifyFailure -> e') ->
                applyAMAP e' (OldestFirst []) nothingApplied
            Right (OldestFirst (undo :| []), pModifier) -> do
                lift $ applyBlocksUnsafe (ShouldCallBListener True) (one (block, undo)) (Just pModifier)
                applyAMAP e (OldestFirst xs) False
            Right _ -> error "verifyAndApplyBlocksInternal: applyAMAP: \
                             \verification of one block produced more than one undo"
    -- Rollbacks and returns an error
    failWithRollback
        :: ApplyBlocksException
        -> [NewestFirst NE Blund]
        -> ExceptT ApplyBlocksException m HeaderHash
    failWithRollback e toRollback = do
        logDebug "verifyAndapply failed, rolling back"
        lift $ mapM_ rollbackBlocks toRollback
        throwError e
    -- This function tries to apply a new portion of blocks (prefix
    -- and suffix). It also has aggregating parameter blunds which is
    -- collected to rollback blocks if correspondent flag is on. First
    -- list is packs of blunds -- head of this list represents blund
    -- to rollback first. This function also tries to apply as much as
    -- possible if the flag is on.
    rollingVerifyAndApply
        :: [NewestFirst NE Blund]
        -> (OldestFirst NE Block, OldestFirst [] Block)
        -> ExceptT ApplyBlocksException m HeaderHash
    rollingVerifyAndApply blunds (prefix, suffix) = do
        let prefixHead = prefix ^. _Wrapped . _neHead
        when (isLeft prefixHead) $ do
            let epochIndex = prefixHead ^. epochIndexL
            logDebug $ "Rolling: Calculating LRC if needed for "
                       <> pretty epochIndex
            lift $ lrcSingleShot epochIndex
        logDebug "Rolling: verifying"
        lift (verifyBlocksPrefix prefix) >>= \case
            Left (ApplyBlocksVerifyFailure -> failure)
                | rollback  -> failWithRollback failure blunds
                | otherwise -> do
                      logDebug "Rolling: Applying AMAP"
                      applyAMAP failure
                                   (over _Wrapped toList prefix)
                                   (null blunds)
            Right (undos, pModifier) -> do
                let newBlunds = OldestFirst $ getOldestFirst prefix `NE.zip`
                                              getOldestFirst undos
                logDebug "Rolling: Verification done, applying unsafe block"
                lift $ applyBlocksUnsafe (ShouldCallBListener True) newBlunds (Just pModifier)
                case getOldestFirst suffix of
                    [] -> GS.getTip
                    (genesis:xs) -> do
                        logDebug "Rolling: Applying done, next portion"
                        rollingVerifyAndApply (toNewestFirst newBlunds : blunds) $
                            spanEpoch (OldestFirst (genesis:|xs))

-- | Apply definitely valid sequence of blocks. At this point we must
-- have verified all predicates regarding block (including txs and ssc
-- data checks). We also must have taken lock on block application
-- and ensured that chain is based on our tip. Blocks will be applied
-- per-epoch, calculating lrc when needed if flag is set.
applyBlocks
    :: forall ctx m.
       (BlockLrcMode ctx m)
    => Bool -> Maybe PollModifier -> OldestFirst NE Blund -> m ()
applyBlocks calculateLrc pModifier blunds = do
    when (isLeft prefixHead && calculateLrc) $
        -- Hopefully this lrc check is never triggered -- because
        -- caller most definitely should have computed lrc to verify
        -- the sequence beforehand.
        lrcSingleShot (prefixHead ^. epochIndexL)
    applyBlocksUnsafe (ShouldCallBListener True) prefix pModifier
    case getOldestFirst suffix of
        []           -> pass
        (genesis:xs) -> applyBlocks calculateLrc pModifier (OldestFirst (genesis:|xs))
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
    :: (MonadBlockApply ctx m)
    => NewestFirst NE Blund -> m ()
rollbackBlocks blunds = do
    tip <- GS.getTip
    let firstToRollback = blunds ^. _Wrapped . _neHead . _1 . headerHashG
    when (tip /= firstToRollback) $
        throwM $ RollbackTipMismatch tip firstToRollback
    rollbackBlocksUnsafe (BypassSecurityCheck False) (ShouldCallBListener True) blunds

-- | Rollbacks some blocks and then applies some blocks.
applyWithRollback
    :: forall ctx m.
    (BlockLrcMode ctx m, MonadMempoolNormalization ctx m)
    => NewestFirst NE Blund        -- ^ Blocks to rollbck
    -> OldestFirst NE Block        -- ^ Blocks to apply
    -> m (Either ApplyBlocksException HeaderHash)
applyWithRollback toRollback toApply = runExceptT $ do
    tip <- GS.getTip
    when (tip /= newestToRollback) $
        throwError $ ApplyBlocksTipMismatch "applyWithRollback/rollback" tip newestToRollback

    let doRollback = rollbackBlocksUnsafe
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
    applyBack = applyBlocks False Nothing reApply
    expectedTipApply = toApply ^. _Wrapped . _neHead . prevBlockL
    newestToRollback = toRollback ^. _Wrapped . _neHead . _1 . headerHashG

    onBadRollback tip =
        applyBack $> Left (ApplyBlocksTipMismatch "applyWithRollback/apply" tip newestToRollback)

    onGoodRollback =
        verifyAndApplyBlocks True toApply >>= \case
            Left err      -> applyBack $> Left err
            Right tipHash -> pure (Right tipHash)
