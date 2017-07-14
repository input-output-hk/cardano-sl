{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

       -- * Lightweight psks handling
       , PskLightVerdict (..)
       , processProxySKLight

       -- * Confirmations
       , ConfirmPskLightVerdict (..)
       , processConfirmProxySk
       , isProxySKConfirmed
       ) where


import           Universum

import           Control.Lens                (at, uses, (%=), (+=), (-=), (.=))
import qualified Data.Cache.LRU              as LRU
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import           Ether.Internal              (HasLens (..))
import           Mockable                    (CurrentTime, Mockable, currentTime)

import           Pos.Binary.Class            (biSize)
import           Pos.Binary.Communication    ()
import           Pos.Constants               (memPoolLimitRatio)
import           Pos.Context                 (lrcActionOnEpochReason)
import           Pos.Core                    (HasPrimaryKey (..), addressHash,
                                              bvdMaxBlockSize, epochIndexL)
import           Pos.Crypto                  (ProxySecretKey (..), PublicKey,
                                              SignTag (SignProxySK), proxyVerify,
                                              toPublic, verifyPsk)
import           Pos.DB                      (MonadDB, MonadDBRead, MonadGState,
                                              MonadRealDB)
import qualified Pos.DB                      as DB
import           Pos.DB.Block                (MonadBlockDB)
import qualified Pos.DB.DB                   as DB
import qualified Pos.DB.GState               as GS
import qualified Pos.DB.Misc                 as Misc
import           Pos.Delegation.Cede         (detectCycleOnAddition, evalMapCede,
                                              pskToDlgEdgeAction)
import           Pos.Delegation.Class        (DlgMemPool, MonadDelegation,
                                              askDelegationState, dwConfirmationCache,
                                              dwEpochId, dwMessageCache, dwPoolSize,
                                              dwProxySKPool, dwThisEpochPosted)
import           Pos.Delegation.Helpers      (isRevokePsk)
import           Pos.Delegation.Logic.Common (DelegationStateAction,
                                              runDelegationStateAction)
import           Pos.Delegation.Types        (DlgPayload, DlgUndo, mkDlgPayload)
import           Pos.Lrc.Context             (LrcContext)
import qualified Pos.Lrc.DB                  as LrcDB
import           Pos.Types                   (ProxySKHeavy, ProxySKLight, ProxySigLight)
import           Pos.Util                    (leftToPanic, microsecondsToUTC)
import qualified Pos.Util.Concurrent.RWLock  as RWL
import qualified Pos.Util.Concurrent.RWVar   as RWV


----------------------------------------------------------------------------
-- Delegation mempool
----------------------------------------------------------------------------

-- | Retrieves current mempool of heavyweight psks plus undo part.
getDlgMempool
    :: (MonadIO m, MonadDBRead m, MonadDelegation ctx m, MonadMask m)
    => m (DlgPayload, DlgUndo)
getDlgMempool = do
    sks <- runDelegationStateAction $
        uses dwProxySKPool HM.elems
    let issuers = map pskIssuerPk sks
    let payload = leftToPanic "getDlgMempool: " $ mkDlgPayload sks
    toRollback <- catMaybes <$> mapM (GS.getPskByIssuer . Left) issuers
    pure (payload, toRollback)

-- | Clears delegation mempool.
clearDlgMemPool
    :: (MonadIO m, MonadDelegation ctx m, MonadMask m)
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

----------------------------------------------------------------------------
-- Heavyweight PSKs processing
----------------------------------------------------------------------------

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
    :: forall ssc ctx m.
       ( MonadIO m
       , MonadMask m
       , MonadDBRead m
       , MonadBlockDB ssc m
       , MonadGState m
       , MonadDelegation ctx m
       , MonadReader ctx m
       , HasLens LrcContext ctx LrcContext
       , Mockable CurrentTime m
       )
    => ProxySKHeavy -> m PskHeavyVerdict
processProxySKHeavy psk = do
    curTime <- microsecondsToUTC <$> currentTime
    headEpoch <- view epochIndexL <$> DB.getTipHeader @ssc
    richmen <-
        toList <$>
        lrcActionOnEpochReason
        headEpoch
        "Delegation.Logic#processProxySKHeavy: there are no richmen for current epoch"
        LrcDB.getRichmenDlg
    maxBlockSize <- bvdMaxBlockSize <$> DB.gsAdoptedBVData
    let msg = Right psk
        consistent = verifyPsk psk
        iPk = pskIssuerPk psk
        -- We don't check stake for revoking certs. You can revoke
        -- even if you don't have money anymore.
        enoughStake = isRevokePsk psk || addressHash iPk `elem` richmen
        omegaCorrect = headEpoch == pskOmega psk
    runDelegationStateAction $ do
        memPoolSize <- use dwPoolSize
        exists <- uses dwProxySKPool (\m -> HM.lookup iPk m == Just psk)
        cached <- isJust . snd . LRU.lookup msg <$> use dwMessageCache
        alreadyPosted <- uses dwThisEpochPosted $ HS.member iPk
        epochMatches <- (headEpoch ==) <$> use dwEpochId
        hasPskInDB <- isJust <$> GS.getPskByIssuer (Left $ pskIssuerPk psk)
        let rerevoke = isRevokePsk psk && not hasPskInDB
        producesCycle <- use dwProxySKPool >>= \pool ->
            -- This is inefficient. Consider supporting this map
            -- in-memory or changing mempool key to stakeholderId.
            let cedeModifier =
                    HM.fromList $
                    map (bimap addressHash pskToDlgEdgeAction) $
                    HM.toList pool
            in lift $ evalMapCede cedeModifier $ detectCycleOnAddition psk
        dwMessageCache %= LRU.insert msg curTime
        let maxMemPoolSize = memPoolLimitRatio * maxBlockSize
            -- Here it would be good to add size of data we want to insert
            -- but it's negligible.
            exhausted = memPoolSize >= maxMemPoolSize
        let res = if | not consistent -> PHBroken
                     | not epochMatches -> PHIncoherent
                     | not omegaCorrect -> PHInvalid "PSK epoch is different from current"
                     | alreadyPosted -> PHInvalid "issuer has already posted PSK this epoch"
                     | rerevoke ->
                         PHInvalid "can't accept revoke cert, user doesn't have any psk in db"
                     | isJust producesCycle ->
                         PHInvalid $ "adding psk causes cycle at: " <> pretty producesCycle
                     | not enoughStake -> PHInvalid "issuer doesn't have enough stake"
                     | cached -> PHCached
                     | exists -> PHExists
                     | exhausted -> PHExhausted
                     | otherwise -> PHAdded
        when (res == PHAdded) $ putToDlgMemPool iPk psk
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
    sk <- view primaryKey
    curTime <- microsecondsToUTC <$> currentTime
    miscLock <- view DB.miscLock <$> DB.getNodeDBs
    psks <- RWL.withRead miscLock Misc.getProxySecretKeys
    res <- runDelegationStateAction $ do
        let pk = toPublic sk
            related = pk == pskDelegatePk psk || pk == pskIssuerPk psk
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
isProxySKConfirmed psk = do
    var <- askDelegationState
    RWV.with var $ \v -> pure $ isJust $ snd $ LRU.lookup psk (v ^. dwConfirmationCache)
