{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | All logic of certificates processing

module Pos.Delegation.Logic
       (
       -- * Helpers
         DelegationStateAction(..)
       , runDelegationStateAction
       , invalidateProxyCaches

       -- * Initialization
       , initDelegation

       -- * Heavyweight psks handling
       , getProxyMempool
       , PskHeavyVerdict (..)
       , processProxySKHeavy
       , delegationApplyBlocks
       , delegationVerifyBlocks
       , delegationRollbackBlocks

       -- * Lightweight psks handling
       , PskLightVerdict (..)
       , processProxySKLight

       -- * Confirmations
       , ConfirmPskLightVerdict (..)
       , processConfirmProxySk
       , isProxySKConfirmed
       ) where

import           Control.Concurrent.STM.TVar (readTVar, writeTVar)
import           Control.Exception           (Exception (..))
import           Control.Lens                (makeLenses, uses, (%=), (.=), _Wrapped)
import           Control.Monad.Trans.Except  (runExceptT, throwE)
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import           Data.List                   (partition)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Text.Buildable         as B
import           Data.Time.Clock             (UTCTime, addUTCTime, getCurrentTime)
import           Formatting                  (bprint, build, sformat, stext, (%))
import           System.Wlog                 (WithLogger)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Block.Types             (Blund, Undo (undoPsk))
import           Pos.Constants               (lightDlgConfirmationTimeout,
                                              messageCacheTimeout)
import           Pos.Context                 (WithNodeContext (getNodeContext),
                                              lrcActionOnEpochReason, ncNodeParams,
                                              npSecretKey)
import           Pos.Crypto                  (ProxySecretKey (..), PublicKey,
                                              pdDelegatePk, proxyVerify, shortHashF,
                                              toPublic, verifyProxySecretKey)
import           Pos.DB                      (DBError (DBMalformed), MonadDB,
                                              SomeBatchOp (..))
import qualified Pos.DB                      as DB
import qualified Pos.DB.Block                as DB
import qualified Pos.DB.DB                   as DB
import qualified Pos.DB.GState               as GS
import qualified Pos.DB.Misc                 as Misc
import           Pos.Delegation.Class        (DelegationWrap, MonadDelegation (..),
                                              dwConfirmationCache, dwEpochId,
                                              dwMessageCache, dwProxySKPool,
                                              dwThisEpochPosted)
import           Pos.Delegation.Types        (SendProxySK (..))
import           Pos.Exception               (cardanoExceptionFromException,
                                              cardanoExceptionToException)
import qualified Pos.Lrc.DB                  as LrcDB
import           Pos.Ssc.Class.Helpers       (SscHelpersClass)
import           Pos.Types                   (Block, HeaderHash, ProxySKHeavy,
                                              ProxySKLight, ProxySigLight, addressHash,
                                              blockProxySKs, epochIndexL, headerHash,
                                              prevBlockL)
import           Pos.Util                    (NE, NewestFirst (..), OldestFirst (..),
                                              withReadLifted, withWriteLifted, _neHead,
                                              _neLast)


----------------------------------------------------------------------------
-- Different helpers to simplify logic
----------------------------------------------------------------------------

-- | Convenient monad to work in 'DelegationWrap' context while being
-- in STM.
newtype DelegationStateAction a = DelegationStateAction
    { getDelegationStateM :: StateT DelegationWrap STM a
    } deriving (Functor, Applicative, Monad, MonadState DelegationWrap)

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
        (res,newState)<- runStateT (getDelegationStateM action) startState
        writeTVar var newState
        pure res

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: UTCTime -> DelegationStateAction ()
invalidateProxyCaches curTime = do
    dwMessageCache %=
        HM.filter (\t -> addUTCTime (toDiffTime messageCacheTimeout) t > curTime)
    dwConfirmationCache %=
        HM.filter (\t -> addUTCTime (toDiffTime lightDlgConfirmationTimeout) t > curTime)
  where
    toDiffTime (t :: Integer) = fromIntegral t

type DelegationWorkMode m = (MonadDelegation m, MonadDB m, WithLogger m)

-- Retrieves psk certificated that have been accumulated before given
-- block. The block itself should be in DB.
getPSKsFromThisEpoch
    :: forall ssc m.
       (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m [ProxySKHeavy]
getPSKsFromThisEpoch tip =
    concatMap (either (const []) (view blockProxySKs)) <$>
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
       (SscHelpersClass ssc, MonadDB m, MonadDelegation m)
    => m ()
initDelegation = do
    tip <- DB.getTipBlockHeader @ssc
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
getProxyMempool
    :: (MonadDB m, MonadDelegation m)
    => m ([ProxySKHeavy], [ProxySKHeavy])
getProxyMempool = do
    sks <- runDelegationStateAction (HM.elems <$> use dwProxySKPool)
    let issuers = map pskIssuerPk sks
    toRollback <- catMaybes <$> mapM GS.getPSKByIssuer issuers
    pure (sks, toRollback)

-- | Datatypes representing a verdict of heavy PSK processing.
data PskHeavyVerdict
    = PHExists       -- ^ If we have exactly the same cert in psk mempool
    | PHInvalid Text -- ^ Can't accept PSK though it's most probably user's error
    | PHBroken       -- ^ Broken (signature, most probably attack, we can ban for this)
    | PHCached       -- ^ Message is cached
    | PHIncoherent   -- ^ Verdict can't be made at the moment (we're updating)
    | PHAdded        -- ^ Successfully processed/added to psk mempool
    deriving (Show,Eq)

-- | Processes heavyweight psk. Puts it into the mempool
-- depending on issuer's stake, overrides if exists, checks
-- validity and cachemsg state.
processProxySKHeavy
    :: forall ssc m.
       ( SscHelpersClass ssc
       , MonadDB m
       , MonadDelegation m
       , WithNodeContext ssc m
       )
    => ProxySKHeavy -> m PskHeavyVerdict
processProxySKHeavy psk = do
    curTime <- liftIO getCurrentTime
    headEpoch <- view epochIndexL <$> DB.getTipBlockHeader @ssc
    richmen <-
        NE.toList <$>
        lrcActionOnEpochReason
        headEpoch
        "Delegation.Logic#processProxySKHeavy: there are no richmen for current epoch"
        LrcDB.getRichmenDlg
    let msg = SendProxySKHeavy psk
        consistent = verifyProxySecretKey psk
        issuer = pskIssuerPk psk
        enoughStake = addressHash issuer `elem` richmen
        omegaCorrect = headEpoch == pskOmega psk
    runDelegationStateAction $ do
        exists <- use dwProxySKPool <&> \m -> HM.lookup issuer m == Just psk
        cached <- HM.member msg <$> use dwMessageCache
        alreadyPosted <- uses dwThisEpochPosted $ HS.member issuer
        epochMatches <- (headEpoch ==) <$> use dwEpochId
        dwMessageCache %= HM.insert msg curTime
        let res = if | not consistent -> PHBroken
                     | not epochMatches -> PHIncoherent
                     | not omegaCorrect -> PHInvalid "PSK epoch is different from current"
                     | alreadyPosted -> PHInvalid "issuer has already posted PSK this epoch"
                     | not enoughStake -> PHInvalid "issuer doesn't have enough stake"
                     | cached -> PHCached
                     | exists -> PHExists
                     | otherwise -> PHAdded
        when (res == PHAdded) $ dwProxySKPool %= HM.insert issuer psk
        pure res

-- state needed for 'delegationVerifyBlocks'
data DelVerState = DelVerState
    { _dvCurEpoch      :: HashSet PublicKey
      -- ^ Set of issuers that have already posted certificates this epoch
    , _dvPSKMapAdded   :: HashMap PublicKey ProxySKHeavy
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
delegationVerifyBlocks
    :: forall ssc m. (SscHelpersClass ssc, MonadDB m, WithNodeContext ssc m)
    => OldestFirst NE (Block ssc)
    -> m (Either Text (OldestFirst NE [ProxySKHeavy]))
delegationVerifyBlocks blocks = do
    -- TODO CSL-502 create snapshot
    tip <- GS.getTip
    fromGenesisPsks <- getPSKsFromThisEpoch @ssc tip
    let _dvCurEpoch = HS.fromList $ map pskIssuerPk fromGenesisPsks
        initState = DelVerState _dvCurEpoch HM.empty HS.empty
    richmen <-
        HS.fromList . NE.toList <$>
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
        let proxySKs = view blockProxySKs blk
            issuers = map pskIssuerPk proxySKs
        when (any (not . (`HS.member` richmen) . addressHash) issuers) $
            throwE $ sformat ("Block "%build%" contains psk issuers that "%
                              "don't have enough stake")
                             (headerHash blk)
        curEpoch <- use dvCurEpoch
        when (any (`HS.member` curEpoch) issuers) $
            throwE $ sformat ("Block "%build%" contains issuers that "%
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
            dwProxySKPool .= HM.empty
            dwThisEpochPosted .= HS.empty
            dwEpochId .= (block ^. epochIndexL)
        pure (SomeBatchOp ([]::[GS.DelegationOp]))
    applyBlock (Right block) = do
        let proxySKs = view blockProxySKs block
            issuers = map pskIssuerPk proxySKs
            (toDelete,toReplace) =
                partition (\ProxySecretKey{..} -> pskIssuerPk == pskDelegatePk)
                proxySKs
            batchOps = map (GS.DelPSK . pskIssuerPk) toDelete ++ map GS.AddPSK toReplace
        runDelegationStateAction $ do
            dwEpochId .= block ^. epochIndexL
            forM_ issuers $ \i -> do
                dwProxySKPool %= HM.delete i
                dwThisEpochPosted %= HS.insert i
        pure $ SomeBatchOp batchOps


-- | Rollbacks block list. Erases mempool of certificates. Better to
-- restore them after the rollback (see Txp#normalizeTxpLD). You can
-- rollback arbitrary number of blocks.
delegationRollbackBlocks
    :: forall ssc m.
       ( SscHelpersClass ssc
       , MonadDelegation m
       , MonadDB m
       , WithNodeContext ssc m
       )
    => NewestFirst NE (Blund ssc) -> m (NonEmpty SomeBatchOp)
delegationRollbackBlocks blunds = do
    tipBlockAfterRollback <-
        maybe (throwM malformedLastParent) pure =<<
        DB.getBlock @ssc (blunds ^. _Wrapped . _neLast . _1 . prevBlockL)
    let epochAfterRollback = tipBlockAfterRollback ^. epochIndexL
    richmen <-
        HS.fromList . NE.toList <$>
        lrcActionOnEpochReason
        (tipBlockAfterRollback ^. epochIndexL)
        "delegationRollbackBlocks: there are no richmen for last rollbacked block epoch"
        LrcDB.getRichmenDlg
    fromGenesisIssuers <-
        HS.fromList . map pskIssuerPk <$> getPSKsFromThisEpoch @ssc tipAfterRollbackHash
    runDelegationStateAction $ do
        dwProxySKPool %=
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
        let proxySKs = view blockProxySKs block
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
    :: (MonadDelegation m, WithNodeContext ssc m, MonadDB m, MonadMask m)
    => ProxySKLight -> m PskLightVerdict
processProxySKLight psk = do
    sk <- npSecretKey . ncNodeParams <$> getNodeContext
    curTime <- liftIO getCurrentTime
    miscLock <- view DB.miscLock <$> DB.getNodeDBs
    psks <- withReadLifted miscLock Misc.getProxySecretKeys
    res <- runDelegationStateAction $ do
        let related = toPublic sk == pskDelegatePk psk
            exists = psk `elem` psks
            msg = SendProxySKLight psk
            valid = verifyProxySecretKey psk
            selfSigned = pskDelegatePk psk == pskIssuerPk psk
        cached <- HM.member msg <$> use dwMessageCache
        dwMessageCache %= HM.insert msg curTime
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
        let valid = proxyVerify (pdDelegatePk proof) proof (const True) psk
        cached <- HM.member psk <$> use dwConfirmationCache
        when valid $ dwConfirmationCache %= HM.insert psk curTime
        pure $ if | cached -> CPCached
                  | not valid -> CPInvalid
                  | otherwise -> CPValid

-- | Checks if we hold a confirmation for given PSK.
isProxySKConfirmed :: ProxySKLight -> DelegationStateAction Bool
isProxySKConfirmed psk = HM.member psk <$> use dwConfirmationCache
