{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Delegation-related verify/apply/rollback part.

module Pos.Delegation.Logic.VAR
       ( dlgVerifyBlocks
       , dlgApplyBlocks
       , dlgRollbackBlocks
       , dlgNormalizeOnRollback
       ) where

import           Universum

import           Control.Lens                 (at, makeLenses, non, uses, (%=), (.=),
                                               (?=), _Wrapped)
import           Control.Monad.Except         (runExceptT, throwError)
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import           Data.List                    (partition, (\\))
import qualified Data.Text.Buildable          as B
import           Formatting                   (bprint, build, sformat, (%))
import           Mockable                     (CurrentTime, Mockable)
import           Serokell.Util                (listJson, mapJson)
import           System.Wlog                  (WithLogger, logDebug)

import           Pos.Binary.Communication     ()
import           Pos.Block.Core               (Block, BlockSignature (..),
                                               mainBlockDlgPayload, mainHeaderLeaderKey,
                                               mcdSignature)
import           Pos.Block.Types              (Blund, Undo (undoDlg))
import           Pos.Context                  (lrcActionOnEpochReason)
import           Pos.Core                     (EpochIndex (..), HasConfiguration,
                                               StakeholderId, addressHash, epochIndexL,
                                               gbHeader, gbhConsensus, headerHash,
                                               prevBlockL)
import           Pos.Crypto                   (ProxySecretKey (..), ProxySignature (..),
                                               psigPsk, shortHashF)
import           Pos.DB                       (DBError (DBMalformed), MonadDBRead,
                                               SomeBatchOp (..))
import qualified Pos.DB                       as DB
import qualified Pos.DB.Block                 as DB
import qualified Pos.DB.DB                    as DB
import           Pos.Delegation.Cede          (CedeModifier, DlgEdgeAction (..), MapCede,
                                               MonadCedeRead (getPsk),
                                               detectCycleOnAddition, dlgEdgeActionIssuer,
                                               dlgReachesIssuance, evalMapCede,
                                               getPskChain, getPskPk, modPsk,
                                               pskToDlgEdgeAction, runDBCede)
import           Pos.Delegation.Class         (MonadDelegation, dwProxySKPool, dwTip)
import           Pos.Delegation.Helpers       (isRevokePsk)
import           Pos.Delegation.Logic.Common  (DelegationError (..),
                                               runDelegationStateAction)
import           Pos.Delegation.Logic.Mempool (clearDlgMemPoolAction,
                                               deleteFromDlgMemPool,
                                               processProxySKHeavyInternal)
import           Pos.Delegation.Types         (DlgPayload (getDlgPayload), DlgUndo (..))
import qualified Pos.GState                   as GS
import           Pos.Lrc.Context              (LrcContext)
import qualified Pos.Lrc.DB                   as LrcDB
import           Pos.Ssc.Class.Helpers        (SscHelpersClass)
import           Pos.Util                     (HasLens', getKeys, _neHead)
import           Pos.Util.Chrono              (NE, NewestFirst (..), OldestFirst (..))


-- Copied from 'these' library.
data These a b = This a | That b | These a b
    deriving (Eq, Show, Generic)

instance (B.Buildable a, B.Buildable b) => B.Buildable (These a b) where
    build (This a)    = bprint ("This {"%build%"}") a
    build (That a)    = bprint ("That {"%build%"}") a
    build (These a b) = bprint ("These {"%build%", "%build%"}") a b

-- u → (Maybe d₁, Maybe d₂): u changed delegate from d₁ (or didn't
-- have one) to d₂ (or revoked delegation). These a b ≃ (Maybe a,
-- Maybe b) w/o (Nothing,Nothing).
type TransChangeset = HashMap StakeholderId (These StakeholderId StakeholderId)
type ReverseTrans = HashMap StakeholderId (HashSet StakeholderId, HashSet StakeholderId)

-- WHENEVER YOU CHANGE THE FUNCTION, CHECK DOCUMENTATION CONSISTENCY! THANK YOU!
--
-- Takes a set of dlg edge actions to apply and returns compensations
-- to dlgTrans and dlgTransRev parts of delegation db. Should be
-- executed under shared Gstate DB lock.
calculateTransCorrections
    :: forall m.
       (MonadDBRead m, WithLogger m)
    => HashSet DlgEdgeAction -> m SomeBatchOp
calculateTransCorrections eActions = do
    -- Get the changeset and convert it to transitive ops.
    changeset <- transChangeset
    let toTransOp iSId (This _)       = GS.DelTransitiveDlg iSId
        toTransOp iSId (That dSId)    = GS.AddTransitiveDlg iSId dSId
        toTransOp iSId (These _ dSId) = GS.AddTransitiveDlg iSId dSId
    let transOps = map (uncurry toTransOp) (HM.toList changeset)

    -- Once we figure out this piece of code works like a charm we
    -- can delete this logging.
    unless (HM.null changeset) $
        logDebug $ sformat ("Nonempty dlg trans changeset: "%mapJson) $
        HM.toList changeset

    -- Bulid reverse transitive set and convert it to reverseTransOps
    let reverseTrans = buildReverseTrans changeset

    let reverseTransIterStep ::
            (StakeholderId, (HashSet StakeholderId, HashSet StakeholderId))
            -> m GS.DelegationOp
        reverseTransIterStep (k, (ad, dl)) = do
            prev <- GS.getDlgTransitiveReverse k
            unless (HS.null $ ad `HS.intersection` dl) $ throwM $ DBMalformed $
                sformat ("Couldn't build reverseOps: ad `intersect` dl is nonzero. "%
                        "ad: "%listJson%", dl: "%listJson)
                        ad dl
            unless (all (`HS.member` prev) dl) $ throwM $
                DBMalformed $
                sformat ("Couldn't build reverseOps: revtrans has "%listJson%
                        ", while "%"trans says we should delete "%listJson)
                        prev dl
            pure $ GS.SetTransitiveDlgRev k $ (prev `HS.difference` dl) <> ad

    reverseOps <- mapM reverseTransIterStep (HM.toList reverseTrans)
    pure $ SomeBatchOp $ transOps <> reverseOps
  where
    {-
    Get the transitive changeset.

    To scare the reader, we suggest the example. Imagine the following
    transformation (all arrows are oriented to the right). First graph
    G is one we store in DB. We apply the list of edgeActions {del DE,
    add CF} ~ F to it. Note that order of applying edgeActions
    matters, so we can't go incrementally building graph G' = G ∪ F
    from G.

    A   F--G      A   F--G
     \             \ /
      C--D--E  ⇒    C  D  E
     /             /
    B             B

    Let dlg_H(a) denote the transitive delegation relation, returning
    Nothing or Just dPk. Then we want to:
    1. Find affected users af = {uv ∈ E(G) ∪ E(F) | dlg_G(u) ≠ dlg_G'(u)}
    2. Calculate new delegate set dlgnew = {(a,dlg_G'(a)), a ∈ af}
    3. Zip dlgnew with old delegates (queried from DB).


    Step 1. Lemma. Let's call x-points XP the set issuers of
    edgeActions set from F. Then af is equal to union of all subtrees
    with root x ∈ XP called U, calculated in graph G'.

    Proof.
    1. a ∈ af ⇒ a ∈ U. Delegate of a changed.
       * (Nothing → Just d) conversion, then we've added some av edge,
         v = d ∨ dlg(v) = d, then a ∈ U.
       * (Just d → Nothing) conversion, we removed direct edge av,
         v = d ∨ dlg(v) = d, same.
       * (Just d₁ → Just d₂), some edge e on path a →→ d₁ was switched
         to another branch or deleted.
    2. a ∈ U ⇒ a ∈ af
       Just come up the tree to the first x ∈ XP.
       * x = a, then we've just either changed or got new/removed
         old delegate.
       * x ≠ a, same.

    So on step 1 it's sufficient to find U. See the code to understand
    how it's done using existent dlgTransRev mapping.


    Step 2. Let's use memoized tree traversal to compute dlgnew. For
    every a ∈ af we'll come to the top of the tree until we see any
    marked value or reach the end.

    1. We've stuck to the end vertex d, which is delegate. Mark dlg(d)
    = Nothing,

    2. We're on u, next vertex is v, we know dlg(v). Set dlg(u) =
    dlg(v) and apply it to all the traversal chain before. If dlg(v) =
    Nothing, set dlg(u) = v instead.


    Step 3 is trivial.
    -}
    transChangeset :: m TransChangeset
    transChangeset = do
        let xPoints :: [StakeholderId]
            xPoints = map dlgEdgeActionIssuer $ HS.toList eActions

        -- Step 1.
        affected <- mconcat <$> mapM calculateLocalAf xPoints
        let af :: HashSet StakeholderId
            af = getKeys affected

        -- Step 2.
        dlgNew <- execStateT (for_ af calculateDlgNew) HM.empty
        -- Let's check that sizes of af and dlgNew match (they should).
        -- We'll need it to merge in (3).
        let notResolved = let dlgKeys = getKeys dlgNew
                          in filter (\k -> not $ HS.member k dlgKeys) $ HS.toList af
        unless (null notResolved) $ throwM $ DBMalformed $
            sformat ("transChangeset: dlgNew keys doesn't resolve some from af: "%listJson)
                    notResolved

        -- Step 3.
        -- Some unsafe functions (чтобы жизнь медом не казалась)
        let lookupUnsafe k =
                fromMaybe (error $ "transChangeset shouldn't happen but happened: " <> pretty k) .
                HM.lookup k
            toTheseUnsafe :: StakeholderId
                          -> (Maybe StakeholderId, Maybe StakeholderId)
                          -> These StakeholderId StakeholderId
            toTheseUnsafe a = \case
                (Nothing,Nothing) ->
                    error $ "Tried to convert (N,N) to These with affected user: " <> pretty a
                (Just x, Nothing) -> This x
                (Nothing, Just x) -> That x
                (Just x, Just y)  -> These x y
        let dlgFin = flip HM.mapWithKey affected $ \a dOld ->
                         toTheseUnsafe a (dOld, lookupUnsafe a dlgNew)

        pure $ dlgFin

    -- Returns map from affected subtree af in original/G to the
    -- common delegate of this subtree. Keys = af. All elems are
    -- similar and equal to dlg(sId).
    calculateLocalAf :: StakeholderId -> m (HashMap StakeholderId (Maybe StakeholderId))
    calculateLocalAf iSId = (HS.toList <$> GS.getDlgTransitiveReverse iSId) >>= \case
        -- We are intermediate/start of the chain, not the delegate.
        [] -> GS.getDlgTransitive iSId >>= \case
            Nothing -> pure $ HM.singleton iSId Nothing
            Just dSId -> do
                -- All i | i →→ d in the G. We should leave only those who
                -- are equal or lower than iPk in the delegation chain.
                revIssuers <- GS.getDlgTransitiveReverse dSId
                -- For these we'll find everyone who's upper (closer to
                -- root/delegate) and take a diff. If iSId = dSId, then it will
                -- return [].
                chain <- getKeys <$> runDBCede (getPskChain iSId)
                let ret = HS.insert iSId
                        (revIssuers `HS.difference` HS.map addressHash chain)
                let retHm = HM.map (const $ Just dSId) $ HS.toMap ret
                pure retHm
        -- We are delegate.
        xs -> pure $ HM.fromList $ (iSId,Nothing):(map (,Just iSId) xs)

    calculateDlgNew :: StakeholderId -> StateT (HashMap StakeholderId (Maybe StakeholderId)) m ()
    calculateDlgNew iSId =
        let resolve v = fmap (addressHash . pskDelegatePk) <$> getPsk v
            -- Sets real new trans delegate in state, returns it to
            -- child. Makes different if we're delegate d -- we set
            -- Nothing, but return d.
            retCached v cont = use (at iSId) >>= \case
                Nothing       -> cont
                Just (Just d) -> pure d
                Just Nothing  -> pure v

            loop :: StakeholderId ->
                    MapCede (StateT (HashMap StakeholderId (Maybe StakeholderId)) m) StakeholderId
            loop v = retCached v $ resolve v >>= \case
                -- There's no delegate = we are the delegate/end of the chain.
                Nothing -> (at v ?= Nothing) $> v
                -- Let's see what's up in the tree
                Just dSId -> do
                    dNew <- loop dSId
                    at v ?= Just dNew
                    pure dNew

            eActionsHM :: CedeModifier
            eActionsHM =
                HM.fromList $ map (\x -> (dlgEdgeActionIssuer x, x)) $
                HS.toList eActions

        in void $ evalMapCede eActionsHM $ loop iSId

    -- Given changeset, returns map d → (ad,dl), where ad is set of
    -- new issuers that delegate to d, while dl is set of issuers that
    -- switched from d to someone else (or to nobody).
    buildReverseTrans :: TransChangeset -> ReverseTrans
    buildReverseTrans changeset =
        let ins = HS.insert
            foldFoo :: ReverseTrans
                    -> StakeholderId
                    -> (These StakeholderId StakeholderId)
                    -> ReverseTrans
            foldFoo rev iSId (This dSId)         = rev & at dSId . non mempty . _2
                                                         %~ (ins iSId)
            foldFoo rev iSId (That dSId)         = rev & at dSId  . non mempty . _1
                                                         %~ (ins iSId)
            foldFoo rev iSId (These dSId1 dSId2) = rev & at dSId1 . non mempty . _2
                                                         %~ (ins iSId)
                                                       & at dSId2 . non mempty . _1
                                                         %~ (ins iSId)
        in HM.foldlWithKey' foldFoo HM.empty changeset

-- This function returns identitifers of stakeholders who are no
-- longer rich in the given epoch, but were rich in the previous one.
getNoLongerRichmen ::
       ( Monad m
       , MonadIO m
       , MonadDBRead m
       , MonadReader ctx m
       , HasLens' ctx LrcContext
       )
    => EpochIndex
    -> m [StakeholderId]
getNoLongerRichmen (EpochIndex 0) = pure mempty
getNoLongerRichmen newEpoch =
    (\\) <$> getRichmen (newEpoch - 1) <*> getRichmen newEpoch
  where
    getRichmen e =
        toList <$>
        lrcActionOnEpochReason e "getNoLongerRichmen" LrcDB.getRichmenDlg

-- State needed for 'delegationVerifyBlocks'.
data DlgVerState = DlgVerState
    { _dvCurEpoch   :: !(HashSet StakeholderId)
      -- ^ Set of issuers that have already posted certificates this epoch
    }

makeLenses ''DlgVerState

-- | Verifies if blocks are correct relatively to the delegation logic
-- and returns a non-empty list of proxySKs needed for undoing
-- them. Predicate for correctness here is:
--
-- * Issuer can post only one cert per epoch
-- * For every new certificate issuer had enough stake at the
--   end of prev. epoch
-- * Delegation payload plus database state doesn't produce cycles.
--
-- It's assumed blocks are correct from 'Pos.Types.Block#verifyBlocks'
-- point of view.
dlgVerifyBlocks ::
       forall ssc ctx m.
       ( DB.MonadBlockDB ssc m
       , MonadIO m
       , MonadReader ctx m
       , HasLens' ctx LrcContext
       , HasConfiguration
       )
    => OldestFirst NE (Block ssc)
    -> m (Either Text (OldestFirst NE DlgUndo))
dlgVerifyBlocks blocks = do
    _dvCurEpoch <- GS.getThisEpochPostedKeys
    let initState = DlgVerState _dvCurEpoch
    (richmen :: HashSet StakeholderId) <-
        lrcActionOnEpochReason
        headEpoch
        "Delegation.Logic#delegationVerifyBlocks: there are no richmen for current epoch"
        LrcDB.getRichmenDlg
    flip evalStateT initState . evalMapCede mempty . runExceptT $
        mapM (verifyBlock richmen) blocks
  where
    headEpoch = blocks ^. _Wrapped . _neHead . epochIndexL

    verifyBlock ::
        HashSet StakeholderId ->
        Block ssc ->
        ExceptT Text (MapCede (StateT DlgVerState m)) DlgUndo
    verifyBlock _ (Left genesisBlk) = do
        prevThisEpochPosted <- use dvCurEpoch
        dvCurEpoch .= HS.empty
        let blkEpoch = genesisBlk ^. epochIndexL
        noLongerRichmen <- lift $ lift $ getNoLongerRichmen blkEpoch
        deletedPSKs <- catMaybes <$> mapM getPsk noLongerRichmen
        -- We should delete all certs for people who are not richmen.
        let delFromCede = modPsk . DlgEdgeDel . addressHash . pskIssuerPk
        mapM_ delFromCede deletedPSKs
        pure $ DlgUndo deletedPSKs prevThisEpochPosted
    verifyBlock richmen (Right blk) = do
        -- We assume here that issuers list doesn't contain
        -- duplicates (checked in payload construction).

        ------------- [Header] -------------

        -- Check 1: Issuer didn't delegate the right to issue to elseone.
        let h = blk ^. gbHeader
        let issuer = h ^. mainHeaderLeaderKey
        let sig = h ^. gbhConsensus ^. mcdSignature
        issuerPsk <- getPskPk issuer
        whenJust issuerPsk $ \psk -> case sig of
            (BlockSignature _) ->
                throwError $
                sformat ("issuer "%build%" has delegated issuance right, "%
                         "so he can't issue the block, psk: "%build%", sig: "%build)
                    issuer psk sig
            _ -> pass

        -- Check 2: Check that if proxy sig is used, delegate indeed
        -- has right to issue the block. Signatures themselves are
        -- checked in the constructor, here we only verify they are
        -- related to slot leader. Self-signed proxySigs are forbidden
        -- on block construction level.
        case h ^. gbhConsensus ^. mcdSignature of
            (BlockPSignatureHeavy pSig) -> do
                let psk = psigPsk pSig
                let delegate = pskDelegatePk psk
                canIssue <- dlgReachesIssuance issuer delegate psk
                unless canIssue $ throwError $
                    sformat ("heavy proxy signature's "%build%" "%
                             "related proxy cert can't be found/doesn't "%
                             "match the one in current allowed heavy psks set")
                            pSig
            (BlockPSignatureLight pSig) -> do
                let pskIPk = pskIssuerPk (psigPsk pSig)
                unless (pskIPk == issuer) $ throwError $
                    sformat ("light proxy signature's "%build%" issuer "%
                             build%" doesn't match block slot leader "%build)
                            pSig pskIPk issuer
            _ -> pass

        ------------- [Payload] -------------

        let proxySKs = getDlgPayload $ view mainBlockDlgPayload blk
            toIssuers = map pskIssuerPk
            allIssuers = toIssuers proxySKs
            allIssuersSt = map addressHash allIssuers
            (revokeIssuers, changeIssuers) =
                bimap toIssuers toIssuers $ partition isRevokePsk proxySKs

        -- Check 3: Issuers have enough money (though it's free to revoke).
        when (any (not . (`HS.member` richmen) . addressHash) changeIssuers) $
            throwError $ sformat ("Block "%build%" contains psk issuers that "%
                                  "don't have enough stake")
                                 (headerHash blk)

        -- Check 4: No issuer has posted psk this epoch before.
        curEpoch <- use dvCurEpoch
        when (any (`HS.member` curEpoch) allIssuersSt) $
            throwError $ sformat ("Block "%build%" contains issuers that "%
                                  "have already published psk this epoch")
                                 (headerHash blk)

        -- Check 5: Every revoking psk indeed revokes previous
        -- non-revoking psk.
        revokePrevCerts <- mapM (\x -> (x,) <$> getPskPk x) revokeIssuers
        let dontHavePrevPsk = filter (isNothing . snd) revokePrevCerts
        unless (null dontHavePrevPsk) $
            throwError $
            sformat ("Block "%build%" contains revoke certs that "%
                     "don't revoke anything: "%listJson)
                     (headerHash blk) (map fst dontHavePrevPsk)

        -- Check 6: applying psks won't create a cycle.
        --
        -- Lemma 1: Removing edges from acyclic graph doesn't create cycles.
        --
        -- Lemma 2: Let G = (E₁,V₁) be acyclic graph and F = (E₂,V₂) another one,
        -- where E₁ ∩ E₂ ≠ ∅ in general case. Then if G ∪ F has a loop C, then
        -- ∃ a ∈ C such that a ∈ E₂.
        --
        -- Hence in order to check whether S=G∪F has cycle, it's sufficient to
        -- validate that dfs won't re-visit any vertex, starting it on
        -- every s ∈ E₂.
        --
        -- In order to do it we should resolve with db, 'dvPskChanged' and
        -- 'proxySKs' together. So it's alright to first apply 'proxySKs'
        -- to 'dvPskChanged' and then perform the check.

        -- Collect rollback info, apply new psks
        changePrevCerts <- mapM getPskPk changeIssuers
        let toRollback = catMaybes $ map snd revokePrevCerts <> changePrevCerts
        mapM_ (modPsk . pskToDlgEdgeAction) proxySKs

        -- Perform the check
        cyclePoints <- catMaybes <$> mapM detectCycleOnAddition proxySKs
        unless (null cyclePoints) $
            throwError $
            sformat ("Block "%build%" leads to psk cycles, at least in these certs: "%listJson)
                    (headerHash blk)
                    (take 5 $ cyclePoints) -- should be enough

        dvCurEpoch %= HS.union (HS.fromList allIssuersSt)
        pure $ DlgUndo toRollback mempty

-- | Applies a sequence of definitely valid blocks to memory state and
-- returns batchops. It works correctly only in case blocks don't
-- cross over epoch. So genesis block is either absent or the head.
dlgApplyBlocks ::
       forall ssc ctx m.
       ( MonadDelegation ctx m
       , MonadIO m
       , MonadDBRead m
       , WithLogger m
       , MonadMask m
       , HasConfiguration
       , SscHelpersClass ssc
       )
    => OldestFirst NE (Blund ssc)
    -> m (NonEmpty SomeBatchOp)
dlgApplyBlocks blunds = do
    tip <- GS.getTip
    let assumedTip = blocks ^. _Wrapped . _neHead . prevBlockL
    when (tip /= assumedTip) $ throwM $
        DelegationCantApplyBlocks $
        sformat
        ("Oldest block is based on tip "%shortHashF%", but our tip is "%shortHashF)
        assumedTip tip
    getOldestFirst <$> mapM applyBlock blunds
  where
    blocks = map fst blunds
    applyBlock :: Blund ssc -> m SomeBatchOp
    applyBlock ((Left block), undoDlg -> DlgUndo{..}) = do
        runDelegationStateAction $ do
            -- all possible psks candidates are now invalid because epoch changed
            clearDlgMemPoolAction
            dwTip .= headerHash block
        -- For genesis blocks, dlg undo is richmen that lost their stake.
        -- So we delete all these guys.
        let edgeActions = map (DlgEdgeDel . addressHash . pskIssuerPk) duPsks
        let edgeOp = SomeBatchOp $ map GS.PskFromEdgeAction edgeActions
        transCorrections <- calculateTransCorrections $ HS.fromList edgeActions
        -- we also should delete all people who posted previous epoch
        let postedOp =
                SomeBatchOp $ map GS.DelPostedThisEpoch $
                HS.toList duPrevEpochPosted
        pure $ edgeOp <> transCorrections <> postedOp
    applyBlock ((Right block), _) = do
        -- for main blocks we can get psks directly from the block,
        -- though it's duplicated in the undo.
        let proxySKs = getDlgPayload $ view mainBlockDlgPayload block
        -- This simple check speeds up empty blocks application a little bit.
        if null proxySKs then pure mempty else do
            let issuers = map pskIssuerPk proxySKs
                edgeActions = map pskToDlgEdgeAction proxySKs
            transCorrections <- calculateTransCorrections $ HS.fromList edgeActions
            let batchOps = SomeBatchOp (map GS.PskFromEdgeAction edgeActions) <> transCorrections
            runDelegationStateAction $ do
                dwTip .= headerHash block
                forM_ issuers deleteFromDlgMemPool
            pure $ SomeBatchOp batchOps


-- | Rollbacks block list. Erases mempool of certificates. Better to
-- restore them after the rollback (see Txp#normalizeTxpLD). You can
-- rollback arbitrary number of blocks.
dlgRollbackBlocks
    :: forall ssc ctx m.
       ( MonadDelegation ctx m
       , DB.MonadBlockDB ssc m
       , WithLogger m
       )
    => NewestFirst NE (Blund ssc) -> m (NonEmpty SomeBatchOp)
dlgRollbackBlocks blunds = do
    getNewestFirst <$> mapM rollbackBlund blunds
  where
    rollbackBlund :: Blund ssc -> m SomeBatchOp
    rollbackBlund (Left _, undoDlg -> DlgUndo{..}) =
        -- We should restore "this epoch posted" set to one from the undo
        pure $ SomeBatchOp $ map GS.AddPostedThisEpoch $ HS.toList duPrevEpochPosted
    rollbackBlund (Right block, undoDlg -> DlgUndo{..}) = do
        let proxySKs = getDlgPayload $ view mainBlockDlgPayload block
            issuers = map pskIssuerPk proxySKs
            backDeleted = issuers \\ map pskIssuerPk duPsks
            edgeActions = map (DlgEdgeDel . addressHash) backDeleted
                       <> map DlgEdgeAdd duPsks
        transCorrections <- calculateTransCorrections $ HS.fromList edgeActions
        let pskOp = SomeBatchOp (map GS.PskFromEdgeAction edgeActions) <> transCorrections
        -- we should also delete issuers from "posted this epoch already"
        let postedOp = SomeBatchOp $ map (GS.DelPostedThisEpoch . addressHash) issuers
        pure $ pskOp <> postedOp

-- | Normalizes the memory state after the rollback. Must be called
-- only when 'StateLock' is taken.
dlgNormalizeOnRollback ::
       forall ssc ctx m.
       ( MonadDelegation ctx m
       , DB.MonadBlockDB ssc m
       , DB.MonadGState m
       , MonadIO m
       , MonadMask m
       , HasLens' ctx LrcContext
       , Mockable CurrentTime m
       , HasConfiguration
       , SscHelpersClass ssc
       )
    => m ()
dlgNormalizeOnRollback = do
    tip <- DB.getTipHeader @ssc
    oldPool <- runDelegationStateAction $ do
        pool <- uses dwProxySKPool toList
        dwProxySKPool .= mempty
        dwTip .= headerHash tip
        pure pool
    forM_ oldPool $ processProxySKHeavyInternal @ssc
