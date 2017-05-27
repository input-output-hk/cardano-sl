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

import           Control.Exception        (Exception (..))
import           Control.Lens             (at, makeLenses, uses, (%%=), (%=), (+=), (-=),
                                           (.=), _Wrapped)
import           Control.Monad.Except     (runExceptT, throwError)
import qualified Data.Cache.LRU           as LRU
import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import           Data.List                (partition)
import qualified Data.Text.Buildable      as B
import           Data.Time.Clock          (UTCTime, addUTCTime, getCurrentTime)
import qualified Ether
import           Formatting               (bprint, build, sformat, stext, (%))
import           System.Wlog              (WithLogger)

import           Pos.Binary.Class         (biSize)
import           Pos.Binary.Communication ()
import           Pos.Block.Core           (Block, mainBlockDlgPayload)
import           Pos.Block.Types          (Blund, Undo (undoPsk))
import           Pos.Constants            (lightDlgConfirmationTimeout, memPoolLimitRatio,
                                           messageCacheTimeout)
import           Pos.Context              (NodeParams (..), lrcActionOnEpochReason)
import           Pos.Core                 (HeaderHash, ProxySKHeavyMap, addressHash,
                                           bvdMaxBlockSize, epochIndexL, headerHash,
                                           prevBlockL)
import           Pos.Crypto               (ProxySecretKey (..), PublicKey,
                                           SignTag (SignProxySK), pdDelegatePk,
                                           proxyVerify, shortHashF, toPublic,
                                           verifyProxySecretKey)
import           Pos.DB                   (DBError (DBMalformed), MonadDB, MonadDBPure,
                                           SomeBatchOp (..))
import qualified Pos.DB                   as DB
import qualified Pos.DB.Block             as DB
import qualified Pos.DB.DB                as DB
import qualified Pos.DB.GState            as GS
import qualified Pos.DB.Misc              as Misc
import           Pos.Delegation.Class     (DelegationWrap (..), DlgMemPool,
                                           MonadDelegation, askDelegationState,
                                           dwConfirmationCache, dwEpochId, dwMessageCache,
                                           dwPoolSize, dwProxySKPool, dwThisEpochPosted)
import           Pos.Delegation.Types     (DlgPayload (getDlgPayload), mkDlgPayload)
import           Pos.Exception            (cardanoExceptionFromException,
                                           cardanoExceptionToException)
import           Pos.Lrc.Context          (LrcContext)
import qualified Pos.Lrc.DB               as LrcDB
import           Pos.Types                (ProxySKHeavy, ProxySKLight, ProxySigLight)
import           Pos.Util                 (withReadLifted, withWriteLifted, _neHead,
                                           _neLast)
import           Pos.Util.Chrono          (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Util.LRU             (filterLRU)
import           Pos.Util.Util            (leftToPanic)

----------------------------------------------------------------------------
-- Different helpers to simplify logic
----------------------------------------------------------------------------

-- | Convenient monad to work in 'DelegationWrap' context while being
-- in STM.
type DelegationStateAction = StateT DelegationWrap STM

-- | Effectively takes a lock on ProxyCaches mvar in NodeContext and
-- allows you to run some computation producing updated ProxyCaches
-- and return value. Will put MVar back on exception.
runDelegationStateAction
    :: (MonadIO m, MonadDelegation m)
    => DelegationStateAction a -> m a
runDelegationStateAction action = do
    var <- askDelegationState
    atomically $ do
        startState <- readTVar var
        (res, newState) <- runStateT action startState
        writeTVar var newState
        pure res

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: UTCTime -> DelegationStateAction ()
invalidateProxyCaches curTime = do
    dwMessageCache %=
        filterLRU (\t -> addUTCTime (toDiffTime messageCacheTimeout) t > curTime)
    dwConfirmationCache %=
        filterLRU (\t -> addUTCTime (toDiffTime lightDlgConfirmationTimeout) t > curTime)
  where
    toDiffTime (t :: Integer) = fromIntegral t

type DelegationWorkMode m = (MonadDelegation m, MonadDB m, WithLogger m, MonadDBPure m)

-- Retrieves psk certificated that have been accumulated before given
-- block. The block itself should be in DB.
getPSKsFromThisEpoch
    :: forall ssc m.
       DB.MonadBlockDB ssc m
    => HeaderHash -> m [ProxySKHeavy]
getPSKsFromThisEpoch tip =
    concatMap (either (const []) (getDlgPayload . view mainBlockDlgPayload)) <$>
        (DB.loadBlocksWhile @ssc) isRight tip

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
       (MonadIO m, DB.MonadBlockDB ssc m, MonadDelegation m)
    => m ()
initDelegation = do
    tip <- DB.getTipHeader @(Block ssc)
    let tipEpoch = tip ^. epochIndexL
    fromGenesisPsks <-
        map pskIssuerPk <$> (getPSKsFromThisEpoch @ssc) (headerHash tip)
    runDelegationStateAction $ do
        dwEpochId .= tipEpoch
        dwThisEpochPosted .= HS.fromList fromGenesisPsks

----------------------------------------------------------------------------
-- Heavyweight PSK
----------------------------------------------------------------------------

-- | Retrieves current mempool of heavyweight psks plus undo part.
getDlgMempool
    :: (MonadIO m, MonadDBPure m, MonadDelegation m)
    => m (DlgPayload, [ProxySKHeavy])
getDlgMempool = do
    sks <- runDelegationStateAction $
        uses dwProxySKPool HM.elems
    let issuers = map pskIssuerPk sks
    let payload = leftToPanic "getDlgMempool: " $ mkDlgPayload sks
    toRollback <- catMaybes <$> mapM GS.getPSKByIssuer issuers
    pure (payload, toRollback)

clearDlgMemPool
    :: (MonadDB m, MonadDelegation m)
    => m ()
clearDlgMemPool = runDelegationStateAction clearDlgMemPoolAction

clearDlgMemPoolAction :: DelegationStateAction ()
clearDlgMemPoolAction = do
    dwProxySKPool .= mempty
    dwPoolSize .= 1

-- Put value into Proxy SK Pool. Value must not exist in pool.
-- Caller must ensure it.
-- Caller must also ensure that size limit allows to put more data.
putToDlgMemPool :: PublicKey -> ProxySKHeavy -> DelegationStateAction ()
putToDlgMemPool pk psk = do
    dwProxySKPool . at pk .= Just psk
    dwPoolSize += biSize pk + biSize psk

deleteFromDlgMemPool :: PublicKey -> DelegationStateAction ()
deleteFromDlgMemPool pk =
    use (dwProxySKPool . at pk) >>= \case
        Nothing -> pass
        Just psk -> do
            dwProxySKPool . at pk .= Nothing
            dwPoolSize -= biSize pk + biSize psk

-- Caller must ensure that there won't be too much data (more than limit) as
-- a result of transformation.
modifyDlgMemPool :: (DlgMemPool -> DlgMemPool) -> DelegationStateAction ()
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
       ( MonadDB m
       , MonadDBPure m
       , DB.MonadBlockDB ssc m
       , DB.MonadDBCore m
       , MonadDelegation m
       , Ether.MonadReader' LrcContext m
       )
    => ProxySKHeavy -> m PskHeavyVerdict
processProxySKHeavy psk = do
    curTime <- liftIO getCurrentTime
    headEpoch <- view epochIndexL <$> DB.getTipHeader @(Block ssc)
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
                     | cached -> PHCached
                     | exists -> PHExists
                     | exhausted -> PHExhausted
                     | otherwise -> PHAdded
        when (res == PHAdded) $ putToDlgMemPool issuer psk
        pure res

-- state needed for 'delegationVerifyBlocks'
data DelVerState = DelVerState
    { _dvCurEpoch      :: HashSet PublicKey
      -- ^ Set of issuers that have already posted certificates this epoch
    , _dvPSKMapAdded   :: ProxySKHeavyMap
      -- ^ Psks added to database.
    , _dvPSKSetRemoved :: HashSet PublicKey
      -- ^ Psks removed from database.
    }

makeLenses ''DelVerState

-- | Verifies if blocks are correct relatively to the delegation logic
-- and returns a non-empty list of proxySKs needed for undoing
-- them. Predicate for correctness here is:
--
-- * Issuer can post only one cert per epoch
-- * For every new certificate issuer had enough stake at the
--   end of prev. epoch
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
    -- TODO CSL-502 create snapshot
    tip <- GS.getTip
    fromGenesisPsks <- getPSKsFromThisEpoch @ssc tip
    let _dvCurEpoch = HS.fromList $ map pskIssuerPk fromGenesisPsks
        initState = DelVerState _dvCurEpoch HM.empty HS.empty
    richmen <-
        HS.fromList . toList <$>
        lrcActionOnEpochReason
        headEpoch
        "Delegation.Logic#delegationVerifyBlocks: there are no richmen for current epoch"
        LrcDB.getRichmenDlg
    when (HS.size _dvCurEpoch /= length fromGenesisPsks) $
        throwM $ DBMalformed "Multiple stakeholders have issued & published psks this epoch"
    evalStateT (runExceptT $ mapM (verifyBlock richmen) blocks) initState
  where
    headEpoch = blocks ^. _Wrapped . _neHead . epochIndexL
    withMapResolve issuer = do
        isAddedM <- HM.lookup issuer <$> use dvPSKMapAdded
        isRemoved <- HS.member issuer <$> use dvPSKSetRemoved
        if isRemoved
        then pure Nothing
        else maybe (GS.getPSKByIssuer issuer) (pure . Just) isAddedM
    withMapAdd psk = do
        let issuer = pskIssuerPk psk
        dvPSKMapAdded %= HM.insert issuer psk
        dvPSKSetRemoved %= HS.delete issuer
    withMapRemove issuer = do
        inAdded <- HM.member issuer <$> use dvPSKMapAdded
        if inAdded
        then dvPSKMapAdded %= HM.delete issuer
        else dvPSKSetRemoved %= HS.insert issuer
    verifyBlock _ (Left _) = do
        dvCurEpoch .= HS.empty
        pure []
    verifyBlock richmen (Right blk) = do
        let proxySKs = getDlgPayload $ view mainBlockDlgPayload blk
            issuers = map pskIssuerPk proxySKs
        when (any (not . (`HS.member` richmen) . addressHash) issuers) $
            throwError $ sformat ("Block "%build%" contains psk issuers that "%
                                  "don't have enough stake")
                                 (headerHash blk)
        curEpoch <- use dvCurEpoch
        when (any (`HS.member` curEpoch) issuers) $
            throwError $ sformat ("Block "%build%" contains issuers that "%
                                  "have already published psk this epoch")
                                 (headerHash blk)
        -- we believe issuers list doesn't contain duplicates,
        -- checked in Types.Block#verifyBlocks
        dvCurEpoch %= HS.union (HS.fromList issuers)
        let toUpdate =
                filter (\ProxySecretKey{..} -> pskIssuerPk /= pskDelegatePk) proxySKs
        toRollback <- catMaybes <$> mapM withMapResolve issuers
        mapM_ withMapRemove issuers
        mapM_ withMapAdd toUpdate
        pure toRollback

-- | Applies a sequence of definitely valid blocks to memory state and
-- returns batchops. It works correctly only in case blocks don't
-- cross over epoch. So genesis block is either absent or the head.
delegationApplyBlocks
    :: forall ssc m. (DelegationWorkMode m)
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
            (toDelete,toReplace) =
                partition (\ProxySecretKey{..} -> pskIssuerPk == pskDelegatePk)
                proxySKs
            batchOps = map (GS.DelPSK . pskIssuerPk) toDelete ++ map GS.AddPSK toReplace
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
    pure $ getNewestFirst (map rollbackBlund blunds)
  where
    malformedLastParent = DBMalformed $
        "delegationRollbackBlocks: parent of last rollbacked block is not in blocks db"
    tipAfterRollbackHash = blunds ^. _Wrapped . _neLast . _1 . prevBlockL
    rollbackBlund :: Blund ssc -> SomeBatchOp
    rollbackBlund (Left _, _) = SomeBatchOp ([]::[GS.DelegationOp])
    rollbackBlund (Right block, undo) =
        let proxySKs = getDlgPayload $ view mainBlockDlgPayload block
            toReplace =
                map pskIssuerPk $
                filter (\ProxySecretKey{..} -> pskIssuerPk /= pskDelegatePk)
                proxySKs
            toDeleteBatch = map GS.DelPSK toReplace
            toAddBatch = map GS.AddPSK $ undoPsk undo
        in SomeBatchOp $ toDeleteBatch ++ toAddBatch


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
    psks <- withReadLifted miscLock Misc.getProxySecretKeys
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
    when (res == PLAdded) $ withWriteLifted miscLock $
        Misc.addProxySecretKey psk
    when (res == PLRemoved) $ withWriteLifted miscLock $
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
    :: (MonadDelegation m, MonadIO m)
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
isProxySKConfirmed :: ProxySKLight -> DelegationStateAction Bool
isProxySKConfirmed psk =
    isJust <$> (dwConfirmationCache %%= swap . LRU.lookup psk)
