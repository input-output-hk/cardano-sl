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
import           Data.Default                   (def)
import qualified Data.HashMap.Strict            as HM
import           Data.Tagged                    (Tagged (..))
import           Formatting                     (build, sformat, (%))
import           System.Wlog                    (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.GodTossing          ()
import           Pos.Core                       (EpochIndex (..), SlotId (..),
                                                 epochIndexL, epochOrSlotG)
import           Pos.DB                         (MonadDBPure, SomeBatchOp (..))
import           Pos.Lrc.Types                  (RichmenStake)
import           Pos.Ssc.Class.Storage          (SscGStateClass (..), SscVerifier)
import           Pos.Ssc.Class.Types            (SscBlock, getSscBlock)
import           Pos.Ssc.Extra                  (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Core        (GtPayload (..), VssCertificatesMap)
import qualified Pos.Ssc.GodTossing.DB          as DB
import           Pos.Ssc.GodTossing.Functions   (getStableCertsPure)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Seed        (calculateSeed)
import           Pos.Ssc.GodTossing.Toss        (MultiRichmenStake, PureToss,
                                                 TossVerFailure (..), applyGenesisBlock,
                                                 rollbackGT, runPureTossWithLogger,
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
    :: (MonadSscMem SscGodTossing m, MonadIO m)
    => m GtGlobalState
gtGetGlobalState = sscRunGlobalQuery ask

getGlobalCerts
    :: (MonadSscMem SscGodTossing m, MonadIO m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        VCD.certs .
        VCD.setLastKnownSlot sl <$>
        view (gsVssCertificates)

-- | Get stable VSS certificates for given epoch.
getStableCerts
    :: (MonadSscMem SscGodTossing m, MonadIO m)
    => EpochIndex -> m VssCertificatesMap
getStableCerts epoch =
    getStableCertsPure epoch <$> sscRunGlobalQuery (view gsVssCertificates)

----------------------------------------------------------------------------
-- Methods from class
----------------------------------------------------------------------------

instance SscGStateClass SscGodTossing where
    sscLoadGlobalState = loadGlobalState
    sscGlobalStateToBatch = Tagged . dumpGlobalState
    sscRollbackU = rollbackBlocks
    sscVerifyAndApplyBlocks = verifyAndApply
    sscCalculateSeedQ _epoch richmen =
        calculateSeed
        <$> view gsCommitments
        <*> view gsOpenings
        <*> view gsShares
        <*> pure richmen

loadGlobalState :: (MonadDBPure m, WithLogger m) => m GtGlobalState
loadGlobalState = do
    logDebug "Loading SSC global state"
    DB.getGtGlobalStateMaybe >>= \case
        Just gs -> gs <$ logInfo (sformat ("Loaded GodTossing state: " %build) gs)
        Nothing -> do
          let vcd = VCD.fromList . toList $ genesisCertificates
          logInfo $ "GodTossing state not found in database, use genesis certificates"
          pure $ def {_gsVssCertificates = vcd}

dumpGlobalState :: GtGlobalState -> [SomeBatchOp]
dumpGlobalState = one . SomeBatchOp . DB.gtGlobalStateToBatch

type GSUpdate a = forall m . (MonadState GtGlobalState m, WithLogger m) => m a

rollbackBlocks :: NewestFirst NE (SscBlock SscGodTossing) -> GSUpdate ()
rollbackBlocks blocks = tossToUpdate mempty $ rollbackGT oldestEOS payloads
  where
    oldestEOS = blocks ^. _Wrapped . _neLast . epochOrSlotG
    payloads = over _Wrapped (map snd . rights . map getSscBlock . toList)
                   blocks

verifyAndApply
    :: RichmenStake
    -> OldestFirst NE (SscBlock SscGodTossing)
    -> SscVerifier SscGodTossing ()
verifyAndApply richmenStake blocks = verifyAndApplyMultiRichmen False richmenData blocks
  where
    epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmenData = HM.fromList [(epoch, richmenStake)]

verifyAndApplyMultiRichmen
    :: Bool
    -> MultiRichmenStake
    -> OldestFirst NE (SscBlock SscGodTossing)
    -> SscVerifier SscGodTossing ()
verifyAndApplyMultiRichmen onlyCerts richmenData =
    tossToVerifier richmenData . mapM_ (verifyAndApplyDo . getSscBlock)
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

tossToUpdate :: MultiRichmenStake -> PureToss a -> GSUpdate a
tossToUpdate richmenData action = do
    oldState <- use identity
    (res, newState) <- runPureTossWithLogger richmenData oldState action
    (identity .= newState) $> res

tossToVerifier
    :: MultiRichmenStake
    -> ExceptT TossVerFailure PureToss a
    -> SscVerifier SscGodTossing a
tossToVerifier richmenData action = do
    oldState <- use identity
    (resOrErr, newState) <-
        runPureTossWithLogger richmenData oldState $ runExceptT action
    case resOrErr of
        Left e    -> throwError e
        Right res -> (identity .= newState) $> res
