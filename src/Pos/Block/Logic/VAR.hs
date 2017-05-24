{-# LANGUAGE ScopedTypeVariables #-}

-- | Verify|apply|rollback logic.

module Pos.Block.Logic.VAR
       ( verifyAndApplyBlocks
       , verifyBlocksPrefix
       , rollbackBlocks
       , applyWithRollback
       ) where

import           Universum

import           Control.Lens             (_Wrapped)
import           Control.Monad.Except     (ExceptT (ExceptT), MonadError (throwError),
                                           runExceptT, withExceptT)
import qualified Data.HashMap.Strict      as HM
import qualified Data.List.NonEmpty       as NE
import qualified Ether
import           Formatting               (build, sformat, (%))
import           Paths_cardano_sl         (version)
import           Serokell.Util.Verify     (formatAllErrors, verResToMonadError)

import           Pos.Block.Core           (Block, GenesisBlock, MainBlock,
                                           genBlockLeaders, mainBlockLeaderKey,
                                           mbTxPayload)
import           Pos.Block.Logic.Internal (applyBlocksUnsafe, rollbackBlocksUnsafe,
                                           toUpdateBlock)
import           Pos.Block.Logic.Util     (tipMismatchMsg)
import qualified Pos.Block.Pure           as Pure
import           Pos.Block.Types          (Blund, Undo (..))
import           Pos.Constants            (lastKnownBlockVersion)
import           Pos.Context              (lrcActionOnEpochReason)
import           Pos.Core                 (BlockVersion (..), HeaderHash, IsGenesisHeader,
                                           IsMainHeader, epochIndexL, gbBody, gbHeader,
                                           headerHash, headerHashG, prevBlockL,
                                           prevBlockL)
import           Pos.DB                   (MonadDBCore)
import qualified Pos.DB.DB                as DB
import qualified Pos.DB.GState            as GS
import           Pos.Delegation.Logic     (delegationVerifyBlocks)
import qualified Pos.Lrc.DB               as LrcDB
import           Pos.Lrc.Worker           (lrcSingleShotNoLock)
import           Pos.Reporting            (reportingFatal)
import           Pos.Slotting.Class       (getCurrentSlot)
import           Pos.Ssc.Class            (SscHelpersClass, SscWorkersClass)
import           Pos.Ssc.Extra            (sscVerifyBlocks)
import           Pos.Txp.Core             (TxPayload)
import           Pos.Txp.Settings         (TxpBlock, TxpGlobalSettings (..))
import qualified Pos.Update.DB            as UDB
import           Pos.Update.Logic         (usVerifyBlocks)
import           Pos.Update.Poll          (PollModifier)
import           Pos.Util                 (Some (Some), neZipWith3, spanSafe, _neHead)
import           Pos.Util.Chrono          (NE, NewestFirst (..), OldestFirst (..),
                                           toNewestFirst, toOldestFirst)
import           Pos.WorkMode.Class       (WorkMode)

-- -- CHECK: @verifyBlocksLogic
-- -- #txVerifyBlocks
-- -- #sscVerifyBlocks
-- -- #delegationVerifyBlocks
-- -- #usVerifyBlocks
-- | Verify new blocks. If parent of the first block is not our tip,
-- verification fails. This function checks everything from block, including
-- header, transactions, delegation data, SSC data, US data.
verifyBlocksPrefix
    :: forall ssc m.
       WorkMode ssc m
    => OldestFirst NE (Block ssc)
    -> m (Either Text (OldestFirst NE Undo, PollModifier))
verifyBlocksPrefix blocks = runExceptT $ do
    curSlot <- getCurrentSlot
    tipBlk <- DB.getTipBlock @ssc
    when (headerHash tipBlk /= blocks ^. _Wrapped . _neHead . prevBlockL) $
        throwError "the first block isn't based on the tip"
    leaders <-
        lrcActionOnEpochReason
        headEpoch
        (sformat
         ("Block.Logic#verifyBlocksPrefix: there are no leaders for epoch "%build) headEpoch)
        LrcDB.getLeaders
    case blocks ^. _Wrapped . _neHead of
        (Left block) ->
            when (block ^. genBlockLeaders /= leaders) $
                throwError "Genesis block leaders don't match with LRC-computed"
        _ -> pass
    (adoptedBV, adoptedBVD) <- UDB.getAdoptedBVFull
    -- We verify that data in blocks is known if protocol version used
    -- by this software is greater than or equal to adopted
    -- version. That's because:
    -- 1. Authors of this software are aware of adopted version.
    -- 2. Each issued block must be formed with respect to adopted version.
    --
    -- Comparison is quite tricky here. Table below demonstrates it.
    --
    --   Our | Adopted | Check?
    -- ————————————————————————
    -- 1.2.3 |  1.2.3  | Yes
    -- 1.2.3 |  1.2.4  | No
    -- 1.2.3 |  1.2.2  | No
    -- 1.2.3 |  1.3.2  | No
    -- 1.2.3 |  1.1.1  | Yes
    -- 2.2.8 |  1.9.9  | Yes
    --
    -- If `(major, minor)` of our version is greater than of adopted
    -- one, then check is certainly done. If it's equal, then check is
    -- done only if `alt` component is the same as adopted one. In
    -- other cases (i. e. when our `(major, minor)` is less than from
    -- adopted version) check is not done.
    let toMajMin BlockVersion {..} = (bvMajor, bvMinor)
    let lastKnownMajMin = toMajMin lastKnownBlockVersion
    let adoptedMajMin = toMajMin adoptedBV
    let dataMustBeKnown = lastKnownMajMin > adoptedMajMin
                       || lastKnownBlockVersion == adoptedBV
    -- For all issuers of blocks we're processing retrieve their PSK
    -- if any and create a hashmap of these.
    pskCerts <-
        fmap (HM.fromList . catMaybes) $
        forM (rights $ NE.toList $ blocks ^. _Wrapped) $ \b ->
        let issuer = b ^. mainBlockLeaderKey
        in fmap (issuer,) <$> GS.getPSKByIssuer issuer
    verResToMonadError formatAllErrors $
        Pure.verifyBlocks curSlot dataMustBeKnown adoptedBVD
        (Just leaders) (Just pskCerts) blocks
    _ <- withExceptT pretty $ sscVerifyBlocks blocks
    TxpGlobalSettings {..} <- Ether.ask'
    txUndo <- withExceptT pretty $ tgsVerifyBlocks dataMustBeKnown $
        map toTxpBlock blocks
    pskUndo <- ExceptT $ delegationVerifyBlocks blocks
    (pModifier, usUndos) <- withExceptT pretty $
        usVerifyBlocks dataMustBeKnown (map toUpdateBlock blocks)
    when (length txUndo /= length pskUndo) $
        throwError "Internal error of verifyBlocksPrefix: lengths of undos don't match"
    pure ( OldestFirst $ neZipWith3 Undo
               (getOldestFirst txUndo)
               (getOldestFirst pskUndo)
               (getOldestFirst usUndos)
         , pModifier)
  where
    headEpoch = blocks ^. _Wrapped . _neHead . epochIndexL

-- [CSL-1156] Need something more elegant, at least eliminate copy-paste.
toTxpBlock
    :: forall ssc.
       SscHelpersClass ssc
    => Block ssc -> TxpBlock
toTxpBlock = bimap convertGenesis convertMain
  where
    convertGenesis :: GenesisBlock ssc -> Some IsGenesisHeader
    convertGenesis = Some . view gbHeader
    convertMain :: MainBlock ssc -> (Some IsMainHeader, TxPayload)
    convertMain blk = (Some $ blk ^. gbHeader, blk ^. gbBody . mbTxPayload)

-- | Applies blocks if they're valid. Takes one boolean flag
-- "rollback". Returns header hash of last applied block (new tip) on
-- success. Failure behaviour depends on "rollback" flag. If it's on,
-- all blocks applied inside this function will be rollbacked, so it
-- will do effectively nothing and return 'Left error'. If it's off,
-- it will try to apply as much blocks as it's possible and return
-- header hash of new tip. It's up to caller to log warning that
-- partial application happened.
verifyAndApplyBlocks
    :: (MonadDBCore m, WorkMode ssc m, SscWorkersClass ssc)
    => Bool -> OldestFirst NE (Block ssc) -> m (Either Text HeaderHash)
verifyAndApplyBlocks rollback =
    reportingFatal version . verifyAndApplyBlocksInternal True rollback

-- See the description for verifyAndApplyBlocks. This method also
-- parameterizes LRC calculation which can be turned on/off with the first
-- flag.
verifyAndApplyBlocksInternal
    :: forall ssc m. (WorkMode ssc m, SscWorkersClass ssc, MonadDBCore m)
    => Bool -> Bool -> OldestFirst NE (Block ssc) -> m (Either Text HeaderHash)
verifyAndApplyBlocksInternal lrc rollback blocks = runExceptT $ do
    tip <- GS.getTip
    let assumedTip = blocks ^. _Wrapped . _neHead . prevBlockL
    when (tip /= assumedTip) $ throwError $
        tipMismatchMsg "verify and apply" tip assumedTip
    rollingVerifyAndApply [] (spanEpoch blocks)
  where
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
            Left e' -> applyAMAP e' (OldestFirst []) nothingApplied
            Right (OldestFirst (undo :| []), pModifier) -> do
                lift $ applyBlocksUnsafe (one (block, undo)) (Just pModifier)
                applyAMAP e (OldestFirst xs) False
            Right _ -> error "verifyAndApplyBlocksInternal: applyAMAP: \
                             \verification of one block produced more than one undo"
    -- Rollbacks and returns an error
    failWithRollback
        :: Text
        -> [NewestFirst NE (Blund ssc)]
        -> ExceptT Text m HeaderHash
    failWithRollback e toRollback = do
        lift $ mapM_ rollbackBlocks toRollback
        throwError e
    -- Calculates LRC if it's needed (no lock)
    calculateLrc epochIx =
        when lrc $ lift $ lrcSingleShotNoLock epochIx
    -- This function tries to apply a new portion of blocks (prefix
    -- and suffix). It also has aggregating parameter blunds which is
    -- collected to rollback blocks if correspondent flag is on. First
    -- list is packs of blunds -- head of this list represents blund
    -- to rollback first. This function also tries to apply as much as
    -- possible if the flag is on.
    rollingVerifyAndApply
        :: [NewestFirst NE (Blund ssc)]
        -> (OldestFirst NE (Block ssc), OldestFirst [] (Block ssc))
        -> ExceptT Text m HeaderHash
    rollingVerifyAndApply blunds (prefix, suffix) = do
        let prefixHead = prefix ^. _Wrapped . _neHead
        when (isLeft prefixHead) $ calculateLrc (prefixHead ^. epochIndexL)
        lift (verifyBlocksPrefix prefix) >>= \case
            Left failure
                | rollback  -> failWithRollback failure blunds
                | otherwise ->
                      applyAMAP failure
                                   (over _Wrapped toList prefix)
                                   (null blunds)
            Right (undos, pModifier) -> do
                let newBlunds = OldestFirst $ getOldestFirst prefix `NE.zip`
                                              getOldestFirst undos
                lift $ applyBlocksUnsafe newBlunds (Just pModifier)
                case getOldestFirst suffix of
                    [] -> GS.getTip
                    (genesis:xs) -> do
                        rollingVerifyAndApply (toNewestFirst newBlunds : blunds) $
                            spanEpoch (OldestFirst (genesis:|xs))

-- | Apply definitely valid sequence of blocks. At this point we must
-- have verified all predicates regarding block (including txs and ssc
-- data checks). We also must have taken lock on block application
-- and ensured that chain is based on our tip. Blocks will be applied
-- per-epoch, calculating lrc when needed if flag is set.
applyBlocks
    :: forall ssc m.
       (MonadDBCore m, WorkMode ssc m, SscWorkersClass ssc)
    => Bool -> Maybe PollModifier -> OldestFirst NE (Blund ssc) -> m ()
applyBlocks calculateLrc pModifier blunds = do
    when (isLeft prefixHead && calculateLrc) $
        -- Hopefully this lrc check is never triggered -- because
        -- caller most definitely should have computed lrc to verify
        -- the sequence beforehand.
        lrcSingleShotNoLock (prefixHead ^. epochIndexL)
    applyBlocksUnsafe prefix pModifier
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
    :: (WorkMode ssc m)
    => NewestFirst NE (Blund ssc) -> m (Maybe Text)
rollbackBlocks blunds = do
    tip <- GS.getTip
    let firstToRollback = blunds ^. _Wrapped . _neHead . _1 . headerHashG
    if tip /= firstToRollback
    then pure $ Just $ tipMismatchMsg "rollback" tip firstToRollback
    else rollbackBlocksUnsafe blunds $> Nothing

-- | Rollbacks some blocks and then applies some blocks.
applyWithRollback
    :: (MonadDBCore m, WorkMode ssc m, SscWorkersClass ssc)
    => NewestFirst NE (Blund ssc)  -- ^ Blocks to rollbck
    -> OldestFirst NE (Block ssc)  -- ^ Blocks to apply
    -> m (Either Text HeaderHash)
applyWithRollback toRollback toApply = reportingFatal version $ runExceptT $ do
    tip <- GS.getTip
    when (tip /= newestToRollback) $ do
        throwError (tipMismatchMsg "rollback in 'apply with rollback'" tip newestToRollback)
    lift $ rollbackBlocksUnsafe toRollback
    tipAfterRollback <- GS.getTip
    when (tipAfterRollback /= expectedTipApply) $ do
        applyBack
        throwError (tipMismatchMsg "apply in 'apply with rollback'" tip newestToRollback)
    lift (verifyAndApplyBlocks True toApply) >>= \case
        -- We didn't succeed to apply blocks, so will apply
        -- rollbacked back.
        Left err -> do
            applyBack
            throwError err
        Right tipHash  -> pure tipHash
  where
    reApply = toOldestFirst toRollback
    applyBack = lift $ applyBlocks True Nothing reApply
    expectedTipApply = toApply ^. _Wrapped . _neHead . prevBlockL
    newestToRollback = toRollback ^. _Wrapped . _neHead . _1 . headerHashG
