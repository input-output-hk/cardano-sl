{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Heavy/lightweight PSK processing, in-mem state and
-- mempool-related functions.

module Pos.Delegation.Logic.Mempool
       (
         -- * Heavyweight psks handling & mempool
         getDlgMempool
       , clearDlgMemPool
       , clearDlgMemPoolAction
       , deleteFromDlgMemPool
       , modifyDlgMemPool

       , PskHeavyVerdict (..)
       , processProxySKHeavy
       , processProxySKHeavyInternal

       -- * Lightweight psks handling
       , PskLightVerdict (..)
       , processProxySKLight

       -- * Confirmations
       , ConfirmPskLightVerdict (..)
       , processConfirmProxySk
       , isProxySKConfirmed
       ) where


import           Universum

import           Control.Lens                     (at, uses, (%=), (+=), (-=), (.=))
import qualified Data.Cache.LRU                   as LRU
import qualified Data.HashMap.Strict              as HM
import           Mockable                         (CurrentTime, Mockable, currentTime)

import           Pos.Binary.Class                 (biSize)
import           Pos.Binary.Communication         ()
import           Pos.Context                      (lrcActionOnEpochReason)
import           Pos.Core                         (HasConfiguration, HasPrimaryKey (..),
                                                   ProxySKHeavy, ProxySKLight,
                                                   ProxySigLight, addressHash,
                                                   bvdMaxBlockSize, epochIndexL,
                                                   getOurPublicKey, headerHash)
import           Pos.Crypto                       (ProxySecretKey (..), PublicKey,
                                                   SignTag (SignProxySK), proxyVerify,
                                                   verifyPsk)
import           Pos.DB                           (MonadDB, MonadDBRead, MonadGState,
                                                   MonadRealDB)
import qualified Pos.DB                           as DB
import           Pos.DB.Block                     (MonadBlockDB)
import qualified Pos.DB.DB                        as DB
import qualified Pos.DB.Misc                      as Misc
import           Pos.Delegation.Cede              (CedeModifier (..), CheckForCycle (..),
                                                   dlgVerifyPskHeavy, evalMapCede,
                                                   pskToDlgEdgeAction)
import           Pos.Delegation.Class             (DlgMemPool, MonadDelegation,
                                                   dwConfirmationCache, dwMessageCache,
                                                   dwPoolSize, dwProxySKPool, dwTip)
import           Pos.Delegation.Helpers           (isRevokePsk)
import           Pos.Delegation.Logic.Common      (DelegationStateAction,
                                                   runDelegationStateAction)
import           Pos.Delegation.Types             (DlgPayload, mkDlgPayload)
import           Pos.Lrc.Context                  (HasLrcContext)
import qualified Pos.Lrc.DB                       as LrcDB
import           Pos.StateLock                    (StateLock, withStateLockNoMetrics)
import           Pos.Util                         (HasLens', leftToPanic,
                                                   microsecondsToUTC)
import           Pos.Util.Concurrent.PriorityLock (Priority (..))
import qualified Pos.Util.Concurrent.RWLock       as RWL

----------------------------------------------------------------------------
-- Delegation mempool
----------------------------------------------------------------------------

-- | Retrieves current mempool of heavyweight psks plus undo part.
getDlgMempool
    :: (MonadIO m, MonadDBRead m, MonadDelegation ctx m, MonadMask m)
    => m DlgPayload
getDlgMempool = do
    sks <- runDelegationStateAction $ uses dwProxySKPool HM.elems
    pure $ leftToPanic "getDlgMempool: " $ mkDlgPayload sks

-- | Clears delegation mempool.
clearDlgMemPool
    :: (MonadIO m, MonadDelegation ctx m, MonadMask m)
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

----------------------------------------------------------------------------
-- Heavyweight PSKs processing
----------------------------------------------------------------------------

-- | Datatypes representing a verdict of heavy PSK processing.
data PskHeavyVerdict
    = PHExists       -- ^ If we have exactly the same cert in psk mempool
    | PHInvalid Text -- ^ Can't accept PSK though it's most probably user's error
    | PHBroken       -- ^ Broken (signature, most probably attack, we can ban for this)
    | PHCached       -- ^ Message is cached
    | PHTipMismatch  -- ^ Verdict can't be made at the moment, mempool tip is different from db one
    | PHExhausted    -- ^ Memory pool is exhausted and can't accept more data
    | PHRemoved      -- ^ Revoked previous psk from the mempool
    | PHAdded        -- ^ Successfully processed/added to psk mempool
    deriving (Show,Eq)

type ProcessHeavyConstraint ctx m =
       ( MonadIO m
       , MonadMask m
       , MonadDBRead m
       , MonadBlockDB m
       , MonadGState m
       , MonadDelegation ctx m
       , MonadReader ctx m
       , HasLrcContext ctx
       , Mockable CurrentTime m
       , HasConfiguration
       )

-- | Processes heavyweight psk. Puts it into the mempool
-- depending on issuer's stake, overrides if exists, checks
-- validity and cachemsg state.
processProxySKHeavy
    :: forall ctx m.
       ( ProcessHeavyConstraint ctx m
       , HasLens' ctx StateLock
       )
    => ProxySKHeavy -> m PskHeavyVerdict
processProxySKHeavy psk =
    withStateLockNoMetrics LowPriority $ \_stateLockHeader ->
        processProxySKHeavyInternal psk

-- | Main logic of heavy psk processing, doesn't have
-- synchronization. Should be called __only__ if you are sure that
-- 'StateLock' is taken already.
processProxySKHeavyInternal ::
       forall ctx m. (ProcessHeavyConstraint ctx m)
    => ProxySKHeavy
    -> m PskHeavyVerdict
processProxySKHeavyInternal psk = do
    curTime <- microsecondsToUTC <$> currentTime
    dbTip <- DB.getTipHeader
    let dbTipHash = headerHash dbTip
    let headEpoch = dbTip ^. epochIndexL
    richmen <-
        lrcActionOnEpochReason
        headEpoch
        "Delegation.Logic#processProxySKHeavy: there are no richmen for current epoch"
        LrcDB.getRichmenDlg
    maxBlockSize <- bvdMaxBlockSize <$> DB.gsAdoptedBVData
    let msg = Right psk
        consistent = verifyPsk psk
        iPk = pskIssuerPk psk

    -- Retrieve psk pool and perform another db check. It's
    -- guaranteed that pool is not changed when we're under
    -- 'withStateLock' lock.
    cyclePool <- runDelegationStateAction $ use dwProxySKPool

    -- This is inefficient. Consider supporting this map
    -- in-memory or changing mempool key to stakeholderId.
    let _cmPskMods = HM.fromList $
            map (bimap addressHash pskToDlgEdgeAction) $
            HM.toList cyclePool
        -- Not used since we can't have more than one psk per issuer
        -- in mempool and "has posted this epoch" is fully backed up
        -- by the database.
    let _cmHasPostedThisEpoch = mempty
    let cedeModifier = CedeModifier {..}
    (verificationError, pskValid) <-
        fmap (either (,False)
                     (const (error "processProxySKHeavyInternal:can't happen",True))) $
        evalMapCede cedeModifier $
        runExceptT $
        dlgVerifyPskHeavy richmen (CheckForCycle True) headEpoch psk

    -- Here the memory state is the same.
    runDelegationStateAction $ do
        memPoolSize <- use dwPoolSize
        posted <- uses dwProxySKPool (\m -> isJust $ HM.lookup iPk m)
        existsSameMempool <- uses dwProxySKPool $ \m -> HM.lookup iPk m == Just psk
        cached <- isJust . snd . LRU.lookup msg <$> use dwMessageCache
        let isRevoke = isRevokePsk psk
        coherent <- uses dwTip $ (==) dbTipHash
        dwMessageCache %= LRU.insert msg curTime

        let -- TODO: This is a rather arbitrary limit, we should
            -- revisit it (see CSL-1664)
            exhausted = memPoolSize >= maxBlockSize * 2

        let res = if | cached -> PHCached
                     | not coherent -> PHTipMismatch
                     | not consistent -> PHBroken
                     | existsSameMempool -> PHExists
                     | not pskValid -> PHInvalid verificationError
                     | exhausted -> PHExhausted
                     | posted && isRevoke -> PHRemoved
                     | otherwise -> PHAdded
        when (res == PHAdded) $ putToDlgMemPool iPk psk
        when (res == PHRemoved) $ deleteFromDlgMemPool iPk
        pure res

----------------------------------------------------------------------------
-- Lightweight PSKs processing
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
processProxySKLight ::
       ( MonadDelegation ctx m
       , MonadReader ctx m
       , HasPrimaryKey ctx
       , MonadDB m
       , MonadMask m
       , MonadRealDB ctx m
       , Mockable CurrentTime m
       )
    => ProxySKLight
    -> m PskLightVerdict
processProxySKLight psk = do
    ourPk <- getOurPublicKey
    curTime <- microsecondsToUTC <$> currentTime
    miscLock <- view DB.miscLock <$> DB.getNodeDBs
    psks <- RWL.withRead miscLock Misc.getProxySecretKeysLight
    res <- runDelegationStateAction $ do
        let related = ourPk == pskDelegatePk psk || ourPk == pskIssuerPk psk
            exists = psk `elem` psks
            msg = Left psk
            valid = verifyPsk psk
            selfSigned = isRevokePsk psk
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
    :: (MonadDelegation ctx m, MonadIO m, MonadMask m, Mockable CurrentTime m)
    => ProxySKLight -> ProxySigLight ProxySKLight -> m ConfirmPskLightVerdict
processConfirmProxySk psk proof = do
    curTime <- microsecondsToUTC <$> currentTime
    runDelegationStateAction $ do
        let valid = proxyVerify SignProxySK proof (const True) psk
        cached <- isJust . snd . LRU.lookup psk <$> use dwConfirmationCache
        when valid $ dwConfirmationCache %= LRU.insert psk curTime
        pure $ if | cached    -> CPCached
                  | valid     -> CPValid
                  | otherwise -> CPInvalid

-- | Checks if we hold a confirmation for given PSK.
isProxySKConfirmed
    :: (MonadIO m, MonadMask m, MonadDelegation ctx m)
    => ProxySKLight -> m Bool
isProxySKConfirmed psk =
    runDelegationStateAction $
        uses dwConfirmationCache $ isJust . snd . LRU.lookup psk
