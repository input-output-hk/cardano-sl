{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | All logic of certificates processing

module Pos.Delegation.Logic
       (
       -- * Helpers
         DelegationStateAction
       , runDelegationStateAction
       , invalidateProxyCaches

       -- * Initialization
       , initDelegation

       -- * Heavyweight psks handling
       , getDlgMempool
       , clearDlgMemPool
       , PskHeavyVerdict (..)
       , processProxySKHeavy
       , delegationVerifyBlocks
       , delegationApplyBlocks
       , delegationRollbackBlocks

       -- * Lightweight psks handling
       , PskLightVerdict (..)
       , processProxySKLight

       -- * Confirmations
       , ConfirmPskLightVerdict (..)
       , processConfirmProxySk
       , isProxySKConfirmed
       ) where


import           Universum

import           Control.Exception          (Exception (..))
import           Control.Lens               (at, lens, makeLenses, uses, (%=), (+=), (-=),
                                             (.=), (?=), _Wrapped)
import           Control.Monad.Except       (runExceptT, throwError)
import qualified Data.Cache.LRU             as LRU
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.List                  (intersect, (\\))
import qualified Data.Text.Buildable        as B
import           Data.Time.Clock            (UTCTime, addUTCTime, getCurrentTime)
import qualified Ether
import           Formatting                 (bprint, build, sformat, stext, (%))
import           Serokell.Util              (listJson)
import           System.Wlog                (WithLogger)

import           Pos.Binary.Class           (biSize)
import           Pos.Binary.Communication   ()
import           Pos.Block.Core             (Block, BlockSignature (..),
                                             mainBlockDlgPayload, mainHeaderLeaderKey,
                                             mcdSignature)
import           Pos.Block.Types            (Blund, Undo (undoPsk))
import           Pos.Constants              (lightDlgConfirmationTimeout,
                                             memPoolLimitRatio, messageCacheTimeout)
import           Pos.Context                (NodeParams (..), lrcActionOnEpochReason)
import           Pos.Core                   (HeaderHash, addressHash, bvdMaxBlockSize,
                                             epochIndexL, gbHeader, gbhConsensus,
                                             headerHash, prevBlockL)
import           Pos.Crypto                 (ProxySecretKey (..), ProxySignature (..),
                                             PublicKey, SignTag (SignProxySK),
                                             pdDelegatePk, proxyVerify, shortHashF,
                                             toPublic, verifyProxySecretKey)
import           Pos.DB                     (DBError (DBMalformed), MonadDB, MonadDBPure,
                                             SomeBatchOp (..))
import qualified Pos.DB                     as DB
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import qualified Pos.DB.GState              as GS
import qualified Pos.DB.Misc                as Misc
import           Pos.Delegation.Class       (DelegationWrap (..), DlgMemPool,
                                             MonadDelegation, askDelegationState,
                                             dwConfirmationCache, dwEpochId,
                                             dwMessageCache, dwPoolSize, dwProxySKPool,
                                             dwThisEpochPosted)
import           Pos.Delegation.Helpers     (dlgMemPoolDetectCycle, dlgReachesIssuance)
import           Pos.Delegation.Types       (DlgPayload (getDlgPayload), mkDlgPayload)
import           Pos.Exception              (cardanoExceptionFromException,
                                             cardanoExceptionToException)
import           Pos.Lrc.Context            (LrcContext)
import qualified Pos.Lrc.DB                 as LrcDB
import           Pos.Ssc.Class.Helpers      (SscHelpersClass)
import           Pos.Types                  (ProxySKHeavy, ProxySKLight, ProxySigLight)
import           Pos.Util                   (leftToPanic, _neHead, _neLast)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))
import qualified Pos.Util.Concurrent.RWLock as RWL
import qualified Pos.Util.Concurrent.RWVar  as RWV
import           Pos.Util.LRU               (filterLRU)

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data DelegationError =
    -- | Can't apply blocks to state of transactions processing.
    DelegationCantApplyBlocks Text
    deriving (Typeable, Show)

instance Exception DelegationError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

instance B.Buildable DelegationError where
    build (DelegationCantApplyBlocks msg) =
        bprint ("can't apply in delegation module: "%stext) msg

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

-- | Initializes delegation in-memory storage.
--
-- * Sets `_dwEpochId` to epoch of tip.
-- * Loads `_dwThisEpochPosted` from database
initDelegation
    :: forall ssc m.
       (MonadIO m, DB.MonadBlockDB ssc m, MonadDelegation m, MonadMask m)
    => m ()
initDelegation = do
    tip <- DB.getTipHeader @ssc
    let tipEpoch = tip ^. epochIndexL
    fromGenesisPsks <-
        map pskIssuerPk <$> (getPSKsFromThisEpoch @ssc) (headerHash tip)
    runDelegationStateAction $ do
        dwEpochId .= tipEpoch
        dwThisEpochPosted .= HS.fromList fromGenesisPsks

----------------------------------------------------------------------------
-- Different helpers to simplify logic
----------------------------------------------------------------------------

-- | Convenient monad to work in 'DelegationWrap' state context.
type DelegationStateAction m = StateT DelegationWrap m

-- | Effectively takes a lock on ProxyCaches mvar in NodeContext and
-- allows you to run some computation producing updated ProxyCaches
-- and return value. Will put MVar back on exception.
runDelegationStateAction
    :: (MonadIO m, MonadMask m, MonadDelegation m)
    => DelegationStateAction m a -> m a
runDelegationStateAction action = do
    var <- askDelegationState
    RWV.modify var $ \startState -> swap <$> runStateT action startState

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: (Monad m) => UTCTime -> DelegationStateAction m ()
invalidateProxyCaches curTime = do
    dwMessageCache %=
        filterLRU (\t -> addUTCTime (toDiffTime messageCacheTimeout) t > curTime)
    dwConfirmationCache %=
        filterLRU (\t -> addUTCTime (toDiffTime lightDlgConfirmationTimeout) t > curTime)
  where
    toDiffTime (t :: Integer) = fromIntegral t

-- Retrieves psk certificated that have been accumulated before given
-- block. The block itself should be in DB.
getPSKsFromThisEpoch
    :: forall ssc m.
       DB.MonadBlockDB ssc m
    => HeaderHash -> m [ProxySKHeavy]
getPSKsFromThisEpoch tip =
    concatMap (either (const []) (getDlgPayload . view mainBlockDlgPayload)) <$>
        (DB.loadBlocksWhile @ssc) isRight tip

-- Returns PSK with supplied issuer, querying both provided mempool
-- (first priority) and the database.
resolveWithDlgDB :: MonadDBPure m => DlgMemPool -> PublicKey -> m (Maybe ProxySKHeavy)
resolveWithDlgDB mp iPk = case HM.lookup iPk mp of
    Nothing -> GS.getPskByIssuer $ Left iPk
    Just s  -> pure $ Just s

-- Copied from 'these' library.
data These a b = This a | That b | These a b
    deriving (Eq, Show, Generic)

-- u → (Maybe d₁, Maybe d₂): u changed delegate from d₁ (or didn't
-- have one) to d₂ (or revoked delegation). These a b ≃ (Maybe a,
-- Maybe b) w/o (Nothing,Nothing).
type TransChangeset = HashMap PublicKey (These PublicKey PublicKey)
type ReverseTrans = HashMap PublicKey (HashSet PublicKey, HashSet PublicKey)

-- WHENEVER YOU CHANGE THE FUNCTION, CHECK DOCUMENTATION CONSISTENCY! THANK YOU!
--
-- Takes a set of dlg edge actions to apply and returns compensations
-- to dlgTrans and dlgTransRev parts of delegation db. Should be
-- executed under shared Gstate DB lock.
calculateTransCorrections
    :: forall m.
       MonadDBPure m
    => HashSet GS.DlgEdgeAction -> m SomeBatchOp
calculateTransCorrections eActions = do
    -- Get the changeset and convert it to transitive ops.
    changeset <- transChangeset
    let toTransOp iPk (This _)      = GS.DelTransitiveDlg iPk
        toTransOp iPk (That dPk)    = GS.AddTransitiveDlg iPk dPk
        toTransOp iPk (These _ dPk) = GS.AddTransitiveDlg iPk dPk
    let transOps = map (uncurry toTransOp) (HM.toList changeset)

    -- Bulid reverse transitive set and convert it to reverseTransOps
    let reverseTrans = buildReverseTrans changeset
    reverseOps <- forM (HM.toList reverseTrans) $ \(k, (ad0, dl0)) -> do
        let ad = HS.toList ad0 -- view patterns are for the weak ones
            dl = HS.toList dl0
        prev <- HS.toList <$> GS.getDlgTransitiveReverse k
        unless (null $ ad `intersect` dl) $ throwM $ DBMalformed $
            sformat ("Couldn't build reverseOps: ad `intersect` dl is nonzero. "%
                     "ad: "%listJson%", dl: "%listJson)
                    ad dl
        unless (all (`elem` prev) dl) $ throwM $ DBMalformed $
            sformat ("Couldn't build reverseOps: revtrans has "%listJson%", while "%
                     "trans says we should delete "%listJson)
                    prev dl
        pure $ GS.SetTransitiveDlgRev k $ HS.fromList $ (prev \\ dl) ++ ad

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
    1. Find affected users af = {e ∈ E_G ∪ E_F | dlg_G(e) ≠ dlg_G'(e)}
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
        let keysMap = HS.fromMap . HM.map (const ())
        let xPoints :: [PublicKey]
            xPoints = map GS.dlgEdgeActionIssuer $ HS.toList eActions

        -- Step 1.
        affected <- mconcat <$> mapM calculateLocalAf xPoints
        let af = keysMap affected

        -- Step 2.
        dlgNew <- execStateT (for_ af calculateDlgNew) HM.empty
        -- Let's check that sizes of af and dlgNew match (they should).
        -- We'll need it to merge in (3).
        let dlgKeys = keysMap dlgNew
        when (dlgKeys /= af) $ throwM $ DBMalformed $
            sformat ("transChangeset: dlgNew keys "%listJson%" /= af "%listJson)
                    (HS.toList dlgKeys) (HS.toList af)

        -- Step 3.
        -- Some unsafe functions (чтобы жизнь медом не казалась)
        let lookupUnsafe k =
                fromMaybe (error $ "transChangeset shouldn't happen but happened: " <> pretty k) .
                HM.lookup k
            toTheseUnsafe :: PublicKey
                          -> (Maybe PublicKey, Maybe PublicKey)
                          -> These PublicKey PublicKey
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
    -- similar and equal to dlg(iPk).
    calculateLocalAf :: PublicKey -> m (HashMap PublicKey (Maybe PublicKey))
    calculateLocalAf iPk = GS.getDlgTransitive iPk >>= \case
        Nothing -> pure $ HM.singleton iPk Nothing
        Just dPk -> do
            -- All i | i → d in the G. We should leave only those who
            -- are lower than iPk in the delegation chain.
            revIssuers <- HS.toList <$> GS.getDlgTransitiveReverse dPk
            -- For these we'll find everyone who's upper (closer to
            -- root/delegate) and take a diff.
            chain <- HM.keys <$> GS.getPskChain (Left iPk)
            let ret = iPk : (revIssuers \\ chain)
            let retHm = HM.fromList $ map (,Just dPk) ret
            -- Just a sanity check. If you're optimizing, delete it.
            unless (HM.size retHm /= length ret) $ throwM $ DBMalformed $
                sformat ("calculateLocalAf for iPk "%build%" has duplicates: "%listJson)
                        iPk ret
            pure retHm

    eActionsHM :: HashMap PublicKey GS.DlgEdgeAction
    eActionsHM =
        HM.fromList $ map (\x -> (GS.dlgEdgeActionIssuer x, x)) $ HS.toList eActions

    calculateDlgNew :: PublicKey -> StateT (HashMap PublicKey (Maybe PublicKey)) m ()
    calculateDlgNew iPk =
        let -- Gets delegate from G': either from 'eActionsHM' or database.
            resolve :: (MonadDBPure n) => PublicKey -> n (Maybe PublicKey)
            resolve v = fmap pskDelegatePk <$> GS.withEActionsResolve eActionsHM v

            -- Sets real new trans delegate in state, returns it to
            -- child. Makes different if we're delegate d -- we set
            -- Nothing, but return d.
            retCached v cont = uses identity (HM.lookup iPk) >>= \case
                Nothing       -> cont
                Just (Just d) -> pure d
                Just Nothing  -> pure v
            loop v = retCached v $ resolve v >>= \case
                -- There's no delegate = we are the delegate/end of the chain.
                Nothing -> (identity . at v ?= Nothing) $> v
                -- Let's see what's up in the tree
                Just dPk -> do
                    dNew <- loop dPk
                    identity . at v ?= Just dNew
                    pure dNew
        in void $ loop iPk

    -- Given changeset, returns map d → (ad,dl), where ad is set of
    -- new issuers that delegate to d, while dl is set of issuers that
    -- switched from d to someone else (or to nobody).
    buildReverseTrans :: TransChangeset -> ReverseTrans
    buildReverseTrans changeset =
        let -- me(mepty). This lens is broken. But it's used in the
            -- way it should be alright.
            _me :: (Monoid a) => Lens' (Maybe a) a
            _me = lens (fromMaybe mempty) (\_ b -> Just b)
            ins = HS.insert
            foldFoo :: ReverseTrans
                    -> PublicKey
                    -> (These PublicKey PublicKey)
                    -> ReverseTrans
            foldFoo rev iPk (This dPk)        = rev & at dPk . _me . _2 %~ (ins iPk)
            foldFoo rev iPk (That dPk)        = rev & at dPk . _me . _1 %~ (ins iPk)
            foldFoo rev iPk (These dPk1 dPk2) = rev & at dPk1 . _me . _1 %~ (ins iPk)
                                                    & at dPk2 . _me . _2 %~ (ins iPk)
        in HM.foldlWithKey' foldFoo HM.empty changeset

----------------------------------------------------------------------------
-- Heavyweight PSK
----------------------------------------------------------------------------

-- | Retrieves current mempool of heavyweight psks plus undo part.
getDlgMempool
    :: (MonadIO m, MonadDBPure m, MonadDelegation m, MonadMask m)
    => m (DlgPayload, [ProxySKHeavy])
getDlgMempool = do
    sks <- runDelegationStateAction $
        uses dwProxySKPool HM.elems
    let issuers = map pskIssuerPk sks
    let payload = leftToPanic "getDlgMempool: " $ mkDlgPayload sks
    toRollback <- catMaybes <$> mapM (GS.getPskByIssuer . Left) issuers
    pure (payload, toRollback)

clearDlgMemPool
    :: (MonadDB m, MonadDelegation m, MonadMask m)
    => m ()
clearDlgMemPool = runDelegationStateAction clearDlgMemPoolAction

clearDlgMemPoolAction :: (Monad m) => DelegationStateAction m ()
clearDlgMemPoolAction = do
    dwProxySKPool .= mempty
    dwPoolSize .= 1

-- Put value into Proxy SK Pool. Value must not exist in pool.
-- Caller must ensure it.
-- Caller must also ensure that size limit allows to put more data.
putToDlgMemPool :: (Monad m) => PublicKey -> ProxySKHeavy -> DelegationStateAction m ()
putToDlgMemPool pk psk = do
    dwProxySKPool . at pk .= Just psk
    dwPoolSize += biSize pk + biSize psk

deleteFromDlgMemPool :: (Monad m) => PublicKey -> DelegationStateAction m ()
deleteFromDlgMemPool pk =
    use (dwProxySKPool . at pk) >>= \case
        Nothing -> pass
        Just psk -> do
            dwProxySKPool . at pk .= Nothing
            dwPoolSize -= biSize pk + biSize psk

-- Caller must ensure that there won't be too much data (more than limit) as
-- a result of transformation.
modifyDlgMemPool :: (Monad m) => (DlgMemPool -> DlgMemPool) -> DelegationStateAction m ()
modifyDlgMemPool f = do
    memPool <- use dwProxySKPool
    let newPool = f memPool
    let newSize = biSize newPool
    dwProxySKPool .= newPool
    dwPoolSize .= newSize

-- | Datatypes representing a verdict of heavy PSK processing.
data PskHeavyVerdict
    = PHExists       -- ^ If we have exactly the same cert in psk mempool
    | PHInvalid Text -- ^ Can't accept PSK though it's most probably user's error
    | PHBroken       -- ^ Broken (signature, most probably attack, we can ban for this)
    | PHCached       -- ^ Message is cached
    | PHIncoherent   -- ^ Verdict can't be made at the moment (we're updating)
    | PHExhausted    -- ^ Memory pool is exhausted and can't accept more data
    | PHAdded        -- ^ Successfully processed/added to psk mempool
    deriving (Show,Eq)

-- | Processes heavyweight psk. Puts it into the mempool
-- depending on issuer's stake, overrides if exists, checks
-- validity and cachemsg state.
processProxySKHeavy
    :: forall ssc m.
       ( SscHelpersClass ssc
       , MonadDB m
       , MonadMask m
       , MonadDBPure m
       , DB.MonadDBCore m
       , MonadDelegation m
       , Ether.MonadReader' LrcContext m
       )
    => ProxySKHeavy -> m PskHeavyVerdict
processProxySKHeavy psk = do
    curTime <- liftIO getCurrentTime
    headEpoch <- view epochIndexL <$> DB.getTipHeader @ssc
    richmen <-
        toList <$>
        lrcActionOnEpochReason
        headEpoch
        "Delegation.Logic#processProxySKHeavy: there are no richmen for current epoch"
        LrcDB.getRichmenDlg
    maxBlockSize <- bvdMaxBlockSize <$> DB.gsAdoptedBVData
    let msg = Right psk
        consistent = verifyProxySecretKey psk
        issuer = pskIssuerPk psk
        enoughStake = addressHash issuer `elem` richmen
        omegaCorrect = headEpoch == pskOmega psk
    runDelegationStateAction $ do
        memPoolSize <- use dwPoolSize
        exists <- uses dwProxySKPool (\m -> HM.lookup issuer m == Just psk)
        cached <- isJust . snd . LRU.lookup msg <$> use dwMessageCache
        alreadyPosted <- uses dwThisEpochPosted $ HS.member issuer
        epochMatches <- (headEpoch ==) <$> use dwEpochId
        producesCycle <- use dwProxySKPool >>= \pool ->
            lift $ dlgMemPoolDetectCycle (resolveWithDlgDB pool) psk
        dwMessageCache %= LRU.insert msg curTime
        let maxMemPoolSize = memPoolLimitRatio * maxBlockSize
            -- Here it would be good to add size of data we want to insert
            -- but it's negligible.
            exhausted = memPoolSize >= maxMemPoolSize
        let res = if | not consistent -> PHBroken
                     | not epochMatches -> PHIncoherent
                     | not omegaCorrect -> PHInvalid "PSK epoch is different from current"
                     | alreadyPosted -> PHInvalid "issuer has already posted PSK this epoch"
                     | not enoughStake -> PHInvalid "issuer doesn't have enough stake"
                     | isJust producesCycle ->
                           PHInvalid $ "adding psk causes cycle at: " <> pretty producesCycle
                     | cached -> PHCached
                     | exists -> PHExists
                     | exhausted -> PHExhausted
                     | otherwise -> PHAdded
        when (res == PHAdded) $ putToDlgMemPool issuer psk
        pure res


-- State needed for 'delegationVerifyBlocks'.
data DelVerState = DelVerState
    { _dvCurEpoch   :: HashSet PublicKey
      -- ^ Set of issuers that have already posted certificates this epoch
    , _dvPskChanged :: HashMap PublicKey GS.DlgEdgeAction
      -- ^ Psks added/removed from the database. Removed psk is
      -- revoked one.
    }

makeLenses ''DelVerState

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
delegationVerifyBlocks ::
       forall ssc m.
       ( DB.MonadBlockDB ssc m
       , DB.MonadDB m
       , Ether.MonadReader' LrcContext m
       )
    => OldestFirst NE (Block ssc)
    -> m (Either Text (OldestFirst NE [ProxySKHeavy]))
delegationVerifyBlocks blocks = do
    tip <- GS.getTip
    fromGenesisPsks <- getPSKsFromThisEpoch @ssc tip
    let _dvCurEpoch = HS.fromList $ map pskIssuerPk fromGenesisPsks
    when (HS.size _dvCurEpoch /= length fromGenesisPsks) $
        throwM $ DBMalformed "Multiple stakeholders have issued & published psks this epoch"
    let initState = DelVerState _dvCurEpoch HM.empty
    richmen <-
        HS.fromList . toList <$>
        lrcActionOnEpochReason
        headEpoch
        "Delegation.Logic#delegationVerifyBlocks: there are no richmen for current epoch"
        LrcDB.getRichmenDlg
    evalStateT (runExceptT $ mapM (verifyBlock richmen) blocks) initState
  where
    headEpoch = blocks ^. _Wrapped . _neHead . epochIndexL
    -- Resolves publicKey into psk with a 'dvPskChanged' map. Doesn't
    -- return revokation certs.
    withMapResolve iPk =
        use dvPskChanged >>= \eActions -> GS.withEActionsResolve eActions iPk
    withMapModify psk =
        dvPskChanged %= HM.insert (pskIssuerPk psk) (GS.pskToDlgEdgeAction psk)
    verifyBlock _ (Left _) = [] <$ (dvCurEpoch .= HS.empty)
    verifyBlock richmen (Right blk) = do
        -- We assume here that issuers list doesn't contain
        -- duplicates (checked in payload construction).

        ------------- [Header] -------------

        -- Check 1: Issuer didn't delegate the right to issue to elseone.
        let h = blk ^. gbHeader
        let issuer = h ^. mainHeaderLeaderKey
        issuerPsk <- withMapResolve issuer
        whenJust issuerPsk $ \psk -> throwError $
            sformat ("issuer "%build%" has delegated issuance right, "%
                     "so he can't issue the block, psk: "%build)
                    issuer psk

        -- Check 2: Issuer has right to issue block using heavyweight
        -- PSK iff he's delegate of slot leader.
        case h ^. gbhConsensus ^. mcdSignature of
            (BlockPSignatureHeavy pSig) -> do
                let delegate = pdDelegatePk pSig
                canIssue <- dlgReachesIssuance withMapResolve issuer delegate (pdCert pSig)
                unless canIssue $ throwError $
                    sformat ("proxy signature's "%build%" related proxy cert "%
                             "can't be found/doesn't match the one in current "%
                             "allowed heavy psks set")
                            pSig
            _ -> pass

        ------------- [Payload] -------------

        let proxySKs = getDlgPayload $ view mainBlockDlgPayload blk
            issuers = map pskIssuerPk proxySKs

        -- Check 3: Issuers have enough money
        when (any (not . (`HS.member` richmen) . addressHash) issuers) $
            throwError $ sformat ("Block "%build%" contains psk issuers that "%
                                  "don't have enough stake")
                                 (headerHash blk)

        -- Check 4: no issuer has posted psk this epoch before.
        curEpoch <- use dvCurEpoch
        when (any (`HS.member` curEpoch) issuers) $
            throwError $ sformat ("Block "%build%" contains issuers that "%
                                  "have already published psk this epoch")
                                 (headerHash blk)

        -- Check 5: applying psks won't create a cycle.
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
        toRollback <- catMaybes <$> mapM withMapResolve issuers
        mapM_ withMapModify proxySKs

        -- Perform check 5.
        cyclePoints <- catMaybes <$> mapM (dlgMemPoolDetectCycle withMapResolve) proxySKs
        unless (null cyclePoints) $
            throwError $
            sformat ("Block "%build%" leads to psk cycles, at least in these certs: "%listJson)
                    (headerHash blk)
                    (take 5 $ cyclePoints) -- should be enough

        dvCurEpoch %= HS.union (HS.fromList issuers)
        pure toRollback

-- | Applies a sequence of definitely valid blocks to memory state and
-- returns batchops. It works correctly only in case blocks don't
-- cross over epoch. So genesis block is either absent or the head.
delegationApplyBlocks
    :: forall ssc m.
       (MonadDelegation m, MonadDB m, MonadDBPure m, WithLogger m, MonadMask m)
    => OldestFirst NE (Block ssc) -> m (NonEmpty SomeBatchOp)
delegationApplyBlocks blocks = do
    tip <- GS.getTip
    let assumedTip = blocks ^. _Wrapped . _neHead . prevBlockL
    when (tip /= assumedTip) $ throwM $
        DelegationCantApplyBlocks $
        sformat
        ("Oldest block is based on tip "%shortHashF%", but our tip is "%shortHashF)
        assumedTip tip
    getOldestFirst <$> mapM applyBlock blocks
  where
    applyBlock :: Block ssc -> m SomeBatchOp
    applyBlock (Left block)      = do
        runDelegationStateAction $ do
            -- all possible psks candidates are now invalid because epoch changed
            clearDlgMemPoolAction
            dwThisEpochPosted .= HS.empty
            dwEpochId .= (block ^. epochIndexL)
        pure (SomeBatchOp ([]::[GS.DelegationOp]))
    applyBlock (Right block) = do
        let proxySKs = getDlgPayload $ view mainBlockDlgPayload block
            issuers = map pskIssuerPk proxySKs
            edgeActions = map GS.pskToDlgEdgeAction proxySKs
        transCorrections <- calculateTransCorrections $ HS.fromList edgeActions
        let batchOps = SomeBatchOp (map GS.PskFromEdgeAction edgeActions) <> transCorrections
        runDelegationStateAction $ do
            dwEpochId .= block ^. epochIndexL
            for_ issuers $ \i -> do
                deleteFromDlgMemPool i
                dwThisEpochPosted %= HS.insert i
        pure $ SomeBatchOp batchOps

-- | Rollbacks block list. Erases mempool of certificates. Better to
-- restore them after the rollback (see Txp#normalizeTxpLD). You can
-- rollback arbitrary number of blocks.
delegationRollbackBlocks
    :: forall ssc m.
       ( MonadDelegation m
       , DB.MonadDB m
       , DB.MonadBlockDB ssc m
       , MonadMask m
       , Ether.MonadReader' LrcContext m
       )
    => NewestFirst NE (Blund ssc) -> m (NonEmpty SomeBatchOp)
delegationRollbackBlocks blunds = do
    tipBlockAfterRollback <-
        maybe (throwM malformedLastParent) pure =<<
        DB.blkGetBlock @ssc (blunds ^. _Wrapped . _neLast . _1 . prevBlockL)
    let epochAfterRollback = tipBlockAfterRollback ^. epochIndexL
    richmen <-
        HS.fromList . toList <$>
        lrcActionOnEpochReason
        (tipBlockAfterRollback ^. epochIndexL)
        "delegationRollbackBlocks: there are no richmen for last rollbacked block epoch"
        LrcDB.getRichmenDlg
    fromGenesisIssuers <-
        HS.fromList . map pskIssuerPk <$> getPSKsFromThisEpoch @ssc tipAfterRollbackHash
    runDelegationStateAction $ do
        modifyDlgMemPool
            (HM.filterWithKey $ \pk psk ->
                 not (pk `HS.member` fromGenesisIssuers) &&
                 (addressHash pk) `HS.member` richmen &&
                 pskOmega psk == epochAfterRollback)
        dwThisEpochPosted .= fromGenesisIssuers
        dwEpochId .= tipBlockAfterRollback ^. epochIndexL
    getNewestFirst <$> mapM rollbackBlund blunds
  where
    malformedLastParent = DBMalformed $
        "delegationRollbackBlocks: parent of last rollbacked block is not in blocks db"
    tipAfterRollbackHash = blunds ^. _Wrapped . _neLast . _1 . prevBlockL
    rollbackBlund :: Blund ssc -> m SomeBatchOp
    rollbackBlund (Left _, _) = pure $ SomeBatchOp ([]::[GS.DelegationOp])
    rollbackBlund (Right block, undo) = do
        let proxySKs = getDlgPayload $ view mainBlockDlgPayload block
            issuers = map pskIssuerPk proxySKs
            toUndo = undoPsk undo
            backDeleted = issuers \\ map pskIssuerPk toUndo
            edgeActions = map GS.DlgEdgeDel backDeleted <> map GS.DlgEdgeAdd toUndo
        transCorrections <- calculateTransCorrections $ HS.fromList edgeActions
        pure $ SomeBatchOp (map GS.PskFromEdgeAction edgeActions) <> transCorrections


----------------------------------------------------------------------------
-- Lightweight PSK propagation
----------------------------------------------------------------------------

-- | PSK check verdict. It can be unrelated (other key or spoiled, no
-- way to differ), exist in storage already or be cached.
data PskLightVerdict
    = PLUnrelated
    | PLInvalid
    | PLExists
    | PLCached
    | PLRemoved
    | PLAdded
    deriving (Show,Eq)

-- TODO CSL-687 Calls to DB are not synchronized for now, because storage is
-- append-only, so nothing bad should happen. But it may be a problem
-- later.
-- | Processes proxy secret key (understands do we need it,
-- adds/caches on decision, returns this decision).
processProxySKLight
    :: (MonadDelegation m, Ether.MonadReader' NodeParams m, MonadDB m, MonadMask m)
    => ProxySKLight -> m PskLightVerdict
processProxySKLight psk = do
    sk <- Ether.asks' npSecretKey
    curTime <- liftIO getCurrentTime
    miscLock <- view DB.miscLock <$> DB.getNodeDBs
    psks <- RWL.withRead miscLock Misc.getProxySecretKeys
    res <- runDelegationStateAction $ do
        let related = toPublic sk == pskDelegatePk psk
            exists = psk `elem` psks
            msg = Left psk
            valid = verifyProxySecretKey psk
            selfSigned = pskDelegatePk psk == pskIssuerPk psk
        cached <- isJust . snd . LRU.lookup msg <$> use dwMessageCache
        dwMessageCache %= LRU.insert msg curTime
        pure $ if | not valid -> PLInvalid
                  | cached -> PLCached
                  | exists -> PLExists
                  | selfSigned -> PLRemoved
                  | not related -> PLUnrelated
                  | otherwise -> PLAdded
    -- (2) We're writing to DB
    when (res == PLAdded) $ RWL.withWrite miscLock $
        Misc.addProxySecretKey psk
    when (res == PLRemoved) $ RWL.withWrite miscLock $
        Misc.removeProxySecretKey $ pskIssuerPk psk
    pure res

----------------------------------------------------------------------------
-- Lightweight PSK confirmation backpropagation
----------------------------------------------------------------------------

-- | Verdict of 'processConfirmProxySk' function
data ConfirmPskLightVerdict
    = CPValid   -- ^ Valid, saved
    | CPInvalid -- ^ Invalid, throw away
    | CPCached  -- ^ Already saved
    deriving (Show,Eq)

-- | Takes a lightweight psk, delegate proof of delivery. Checks if
-- it's valid or not. Caches message in any case.
processConfirmProxySk
    :: (MonadDelegation m, MonadIO m, MonadMask m)
    => ProxySKLight -> ProxySigLight ProxySKLight -> m ConfirmPskLightVerdict
processConfirmProxySk psk proof = do
    curTime <- liftIO getCurrentTime
    runDelegationStateAction $ do
        let valid = proxyVerify SignProxySK
                      (pdDelegatePk proof)
                      proof
                      (const True)
                      psk
        cached <- isJust . snd . LRU.lookup psk <$> use dwConfirmationCache
        when valid $ dwConfirmationCache %= LRU.insert psk curTime
        pure $ if | cached    -> CPCached
                  | valid     -> CPValid
                  | otherwise -> CPInvalid

-- | Checks if we hold a confirmation for given PSK.
isProxySKConfirmed
    :: (MonadIO m, MonadMask m, MonadDelegation m)
    => ProxySKLight -> m Bool
isProxySKConfirmed psk = do
    var <- askDelegationState
    RWV.with var $ \v -> pure $ isJust $ snd $ LRU.lookup psk (v ^. dwConfirmationCache)
