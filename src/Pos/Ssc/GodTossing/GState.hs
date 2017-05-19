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
import qualified Data.HashMap.Strict            as HM
import           Data.Tagged                    (Tagged (..))
import           Formatting                     (build, sformat, (%))
import           System.Wlog                    (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.Ssc                 ()
import           Pos.DB                         (MonadDB, SomeBatchOp (..))
import           Pos.Lrc.Types                  (RichmenStake)
import           Pos.Ssc.Class.Storage          (SscGStateClass (..), SscVerifier)
import           Pos.Ssc.Extra                  (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Core        (GtPayload (..), VssCertificatesMap)
import qualified Pos.Ssc.GodTossing.DB          as DB
import           Pos.Ssc.GodTossing.Functions   (getStableCertsPure)
import           Pos.Ssc.GodTossing.Seed        (calculateSeed)
import           Pos.Ssc.GodTossing.Toss        (MultiRichmenStake, PureToss,
                                                 TossVerFailure (..), applyGenesisBlock,
                                                 rollbackGT, runPureTossWithLogger,
                                                 verifyAndApplyGtPayload)
import           Pos.Ssc.GodTossing.Type        (SscGodTossing)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..), gsCommitments,
                                                 gsOpenings, gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (Block, EpochIndex (..), SlotId (..),
                                                 blockMpc, epochIndexL, epochOrSlotG,
                                                 gbHeader)
import           Pos.Util                       (_neHead, _neLast)
import           Pos.Util.Chrono                (NE, NewestFirst (..), OldestFirst (..))

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

loadGlobalState :: (MonadDB m, WithLogger m) => m GtGlobalState
loadGlobalState = do
    logDebug "Loading SSC global state"
    gs <- DB.getGtGlobalState
    gs <$ logInfo (sformat ("Loaded GodTossing state: " %build) gs)

dumpGlobalState :: GtGlobalState -> [SomeBatchOp]
dumpGlobalState = one . SomeBatchOp . DB.gtGlobalStateToBatch

type GSUpdate a = forall m . (MonadState GtGlobalState m, WithLogger m) => m a

rollbackBlocks :: NewestFirst NE (Block SscGodTossing) -> GSUpdate ()
rollbackBlocks blocks = tossToUpdate mempty $ rollbackGT oldestEOS payloads
  where
    oldestEOS = blocks ^. _Wrapped . _neLast . epochOrSlotG
    payloads =
        NewestFirst . map (view blockMpc) . rights . toList $ blocks

verifyAndApply
    :: RichmenStake
    -> OldestFirst NE (Block SscGodTossing)
    -> SscVerifier SscGodTossing ()
verifyAndApply richmenStake blocks = verifyAndApplyMultiRichmen False richmenData blocks
  where
    epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmenData = HM.fromList [(epoch, richmenStake)]

verifyAndApplyMultiRichmen
    :: Bool
    -> MultiRichmenStake
    -> OldestFirst NE (Block SscGodTossing)
    -> SscVerifier SscGodTossing ()
verifyAndApplyMultiRichmen onlyCerts richmenData =
    tossToVerifier richmenData . mapM_ verifyAndApplyDo
  where
    verifyAndApplyDo (Left blk) = applyGenesisBlock $ blk ^. epochIndexL
    verifyAndApplyDo (Right blk) =
        verifyAndApplyGtPayload (Right $ blk ^. gbHeader) $ filterPayload (blk ^. blockMpc)
    filterPayload payload | onlyCerts = leaveOnlyCerts payload
                          | otherwise = payload
    leaveOnlyCerts (CommitmentsPayload _ certs) = CommitmentsPayload mempty certs
    leaveOnlyCerts (OpeningsPayload _ certs)    = OpeningsPayload mempty certs
    leaveOnlyCerts (SharesPayload _ certs)      = SharesPayload mempty certs
    leaveOnlyCerts c@(CertificatesPayload _)    = c

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
