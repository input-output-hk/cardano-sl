{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Heavy PSK processing, in-memory state and mempool-related functions.

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
       ) where


import           Universum

import           Control.Lens (at, uses, (%=), (+=), (-=), (.=))
import qualified Data.Cache.LRU as LRU
import qualified Data.HashMap.Strict as HM
import           Mockable (CurrentTime, Mockable, currentTime)
import           UnliftIO (MonadUnliftIO)

import           Pos.Binary.Class (biSize)
import           Pos.Core (ProxySKHeavy, addressHash, bvdMaxBlockSize, HasProtocolMagic,
                           epochIndexL, headerHash, HasGenesisBlockVersionData)
import           Pos.Crypto (ProxySecretKey (..), PublicKey)
import           Pos.DB (MonadDBRead, MonadGState)
import qualified Pos.DB as DB
import           Pos.Delegation.Cede (CheckForCycle (..), cmPskMods, dlgVerifyPskHeavy,
                                      emptyCedeModifier, evalMapCede, pskToDlgEdgeAction)
import           Pos.Delegation.Class (DlgMemPool, MonadDelegation, dwMessageCache, dwPoolSize,
                                       dwProxySKPool, dwTip)
import           Pos.Delegation.Logic.Common (DelegationStateAction, runDelegationStateAction)
import           Pos.Delegation.Lrc (getDlgRichmen)
import           Pos.Delegation.Types (DlgPayload (..), isRevokePsk)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.StateLock (StateLock, withStateLockNoMetrics)
import           Pos.Util (HasLens', microsecondsToUTC)
import           Pos.Util.Concurrent.PriorityLock (Priority (..))

----------------------------------------------------------------------------
-- Delegation mempool
----------------------------------------------------------------------------

-- | Retrieves current mempool of heavyweight psks plus undo part.
getDlgMempool
    :: (MonadIO m, MonadDBRead m, MonadDelegation ctx m, MonadMask m)
    => m DlgPayload
getDlgMempool = UnsafeDlgPayload <$> (runDelegationStateAction $ uses dwProxySKPool HM.elems)

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
    | PHCached       -- ^ Message is cached
    | PHTipMismatch  -- ^ Verdict can't be made at the moment, mempool tip is different from db one
    | PHExhausted    -- ^ Memory pool is exhausted and can't accept more data
    | PHRemoved      -- ^ Revoked previous psk from the mempool
    | PHAdded        -- ^ Successfully processed/added to psk mempool
    deriving (Show,Eq)

type ProcessHeavyConstraint ctx m =
       ( MonadIO m
       , MonadUnliftIO m
       , MonadMask m
       , MonadDBRead m
       , MonadGState m
       , MonadDelegation ctx m
       , MonadReader ctx m
       , HasLrcContext ctx
       , Mockable CurrentTime m
       )

-- | Processes heavyweight psk. Puts it into the mempool
-- depending on issuer's stake, overrides if exists, checks
-- validity and cachemsg state.
processProxySKHeavy
    :: forall ctx m.
       ( ProcessHeavyConstraint ctx m
       , HasLens' ctx StateLock
       , HasGenesisBlockVersionData
       , HasProtocolMagic
       )
    => ProxySKHeavy -> m PskHeavyVerdict
processProxySKHeavy psk =
    withStateLockNoMetrics LowPriority $ \_stateLockHeader ->
        processProxySKHeavyInternal psk

-- | Main logic of heavy psk processing, doesn't have
-- synchronization. Should be called __only__ if you are sure that
-- 'StateLock' is taken already.
processProxySKHeavyInternal ::
       forall ctx m. (ProcessHeavyConstraint ctx m, HasGenesisBlockVersionData, HasProtocolMagic)
    => ProxySKHeavy
    -> m PskHeavyVerdict
processProxySKHeavyInternal psk = do
    curTime <- microsecondsToUTC <$> currentTime
    dbTip <- DB.getTipHeader
    let dbTipHash = headerHash dbTip
    let headEpoch = dbTip ^. epochIndexL
    richmen <- getDlgRichmen "Delegation.Logic#processProxySKHeavy" headEpoch
    maxBlockSize <- bvdMaxBlockSize <$> DB.gsAdoptedBVData
    let iPk = pskIssuerPk psk

    -- Retrieve psk pool and perform another db check. It's
    -- guaranteed that pool is not changed when we're under
    -- 'withStateLock' lock.
    cyclePool <- runDelegationStateAction $ use dwProxySKPool

    -- This is inefficient. Consider supporting this map
    -- in-memory or changing mempool key to stakeholderId.
    let pskMods = HM.fromList $
            map (bimap addressHash pskToDlgEdgeAction) $
            HM.toList cyclePool
    -- We don't use postedThisEpoch since we can't have more than
    -- one psk per issuer in mempool and "has posted this epoch"
    -- is fully backed up by the database.
    let cedeModifier = emptyCedeModifier & cmPskMods .~ pskMods
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
        cached <- isJust . snd . LRU.lookup psk <$> use dwMessageCache
        let isRevoke = isRevokePsk psk
        coherent <- uses dwTip $ (==) dbTipHash
        dwMessageCache %= LRU.insert psk curTime

        let -- TODO: This is a rather arbitrary limit, we should
            -- revisit it (see CSL-1664)
            exhausted = memPoolSize >= maxBlockSize * 2

        let res = if | cached -> PHCached
                     | not coherent -> PHTipMismatch
                     | existsSameMempool -> PHExists
                     | not pskValid -> PHInvalid verificationError
                     | exhausted -> PHExhausted
                     | posted && isRevoke -> PHRemoved
                     | otherwise -> PHAdded
        when (res == PHAdded) $ putToDlgMemPool iPk psk
        when (res == PHRemoved) $ deleteFromDlgMemPool iPk
        pure res
