{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Instance of SscGStateClass.

module Pos.Ssc.GodTossing.GState
       ( -- * Instances
         -- ** instance SscGStateClass SscGodTossing
         getGlobalCerts
       , gtGetGlobalState
       , getStableCerts
       ) where

import           Control.Lens                   ((.=), _Wrapped)
import           Control.Monad.Except           (MonadError (throwError), runExceptT)
import           Control.Monad.Morph            (hoist)
import qualified Crypto.Random                  as Rand
import qualified Data.HashMap.Strict            as HM
import           Data.Tagged                    (Tagged (..))
import           Formatting                     (build, sformat, (%))
import           System.Wlog                    (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.GodTossing          ()
import           Pos.Core                       (BlockVersionData, EpochIndex (..),
                                                 HasConfiguration, SlotId (..),
                                                 VssCertificatesMap, epochIndexL,
                                                 epochOrSlotG, vcVssKey)
import           Pos.DB                         (MonadDBRead, SomeBatchOp (..))
import           Pos.Lrc.Types                  (RichmenStakes)
import           Pos.Ssc.Class.Storage          (SscGStateClass (..), SscVerifier)
import           Pos.Ssc.Class.Types            (SscBlock, getSscBlock)
import           Pos.Ssc.Extra                  (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Ssc.GodTossing.Core        (GtPayload (..))
import qualified Pos.Ssc.GodTossing.DB          as DB
import           Pos.Ssc.GodTossing.Functions   (getStableCertsPure)
import           Pos.Ssc.GodTossing.Seed        (calculateSeed)
import           Pos.Ssc.GodTossing.Toss        (MultiRichmenStakes, PureToss,
                                                 TossVerFailure (..), applyGenesisBlock,
                                                 rollbackGT, runPureTossWithLogger,
                                                 supplyPureTossEnv,
                                                 verifyAndApplyGtPayload)
import           Pos.Ssc.GodTossing.Type        (SscGodTossing)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..), gsCommitments,
                                                 gsOpenings, gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Util.Chrono                (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Util.Util                  (_neHead, _neLast)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

gtGetGlobalState
    :: (MonadSscMem SscGodTossing ctx m, MonadIO m)
    => m GtGlobalState
gtGetGlobalState = sscRunGlobalQuery ask

getGlobalCerts
    :: (MonadSscMem SscGodTossing ctx m, MonadIO m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        VCD.certs .
        VCD.setLastKnownSlot sl <$>
        view gsVssCertificates

-- | Get stable VSS certificates for given epoch.
getStableCerts
    :: (HasGtConfiguration, HasConfiguration, MonadSscMem SscGodTossing ctx m, MonadIO m)
    => EpochIndex -> m VssCertificatesMap
getStableCerts epoch =
    getStableCertsPure epoch <$> sscRunGlobalQuery (view gsVssCertificates)

----------------------------------------------------------------------------
-- Methods from class
----------------------------------------------------------------------------

instance (HasGtConfiguration, HasConfiguration) => SscGStateClass SscGodTossing where
    sscLoadGlobalState = loadGlobalState
    sscGlobalStateToBatch = Tagged . dumpGlobalState
    sscRollbackU = rollbackBlocks
    sscVerifyAndApplyBlocks = verifyAndApply
    sscCalculateSeedQ _epoch richmen =
        calculateSeed
        <$> view gsCommitments
        <*> (map vcVssKey . VCD.certs <$> view gsVssCertificates)
        <*> view gsOpenings
        <*> view gsShares
        <*> pure richmen

loadGlobalState :: (HasConfiguration, MonadDBRead m, WithLogger m) => m GtGlobalState
loadGlobalState = do
    logDebug "Loading SSC global state"
    gs <- DB.getGtGlobalState
    gs <$ logInfo (sformat ("Loaded GodTossing state: " %build) gs)

dumpGlobalState :: HasConfiguration => GtGlobalState -> [SomeBatchOp]
dumpGlobalState = one . SomeBatchOp . DB.gtGlobalStateToBatch

-- randomness needed for crypto :(
type GSUpdate a = forall m . (MonadState GtGlobalState m, WithLogger m, Rand.MonadRandom m) => m a

rollbackBlocks :: (HasGtConfiguration, HasConfiguration) => NewestFirst NE (SscBlock SscGodTossing) -> GSUpdate ()
rollbackBlocks blocks = tossToUpdate $ rollbackGT oldestEOS payloads
  where
    oldestEOS = blocks ^. _Wrapped . _neLast . epochOrSlotG
    payloads = over _Wrapped (map snd . rights . map getSscBlock . toList)
                   blocks

verifyAndApply
    :: (HasGtConfiguration, HasConfiguration)
    => RichmenStakes
    -> BlockVersionData
    -> OldestFirst NE (SscBlock SscGodTossing)
    -> SscVerifier SscGodTossing ()
verifyAndApply richmenStake bvd blocks =
    verifyAndApplyMultiRichmen False (richmenData, bvd) blocks
  where
    epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmenData = HM.fromList [(epoch, richmenStake)]

verifyAndApplyMultiRichmen
    :: (HasGtConfiguration, HasConfiguration)
    => Bool
    -> (MultiRichmenStakes, BlockVersionData)
    -> OldestFirst NE (SscBlock SscGodTossing)
    -> SscVerifier SscGodTossing ()
verifyAndApplyMultiRichmen onlyCerts env =
    tossToVerifier . hoist (supplyPureTossEnv env) .
    mapM_ (verifyAndApplyDo . getSscBlock)
  where
    verifyAndApplyDo (Left header) = applyGenesisBlock $ header ^. epochIndexL
    verifyAndApplyDo (Right (header, payload)) =
        verifyAndApplyGtPayload (Right header) $
        filterPayload payload
    filterPayload payload
        | onlyCerts = leaveOnlyCerts payload
        | otherwise = payload
    leaveOnlyCerts (CommitmentsPayload _ certs) =
        CommitmentsPayload mempty certs
    leaveOnlyCerts (OpeningsPayload _ certs) = OpeningsPayload mempty certs
    leaveOnlyCerts (SharesPayload _ certs) = SharesPayload mempty certs
    leaveOnlyCerts c@(CertificatesPayload _) = c

tossToUpdate :: PureToss a -> GSUpdate a
tossToUpdate action = do
    oldState <- use identity
    (res, newState) <- runPureTossWithLogger oldState action
    (identity .= newState) $> res

tossToVerifier
    :: ExceptT TossVerFailure PureToss a
    -> SscVerifier SscGodTossing a
tossToVerifier action = do
    oldState <- use identity
    (resOrErr, newState) <-
        runPureTossWithLogger oldState $ runExceptT action
    case resOrErr of
        Left e    -> throwError e
        Right res -> (identity .= newState) $> res
