-- | Global state logic.

module Pos.Ssc.GodTossing.GState
       (
       ) where

import           Control.Lens                   ((%=), (.=), (<>=), _Wrapped)
import           Control.Monad.Except           (MonadError (throwError), runExceptT)
import           Control.Monad.Reader           (ask)
import           Control.Monad.State            (get)
import           Data.Default                   (def)
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Data.List.NonEmpty             as NE
import           Formatting                     (build, sformat, (%))
import           Serokell.Util.Text             (listJson)
import           System.Wlog                    (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.Ssc                 ()
import           Pos.Constants                  (epochSlots, vssMaxTTL)
import           Pos.DB                         (DBError (DBMalformed), MonadDB,
                                                 getTipBlockHeader,
                                                 loadBlundsFromTipWhile)
import           Pos.Lrc.Types                  (Richmen)
import           Pos.Ssc.Class.Storage          (SscStorageClass (..))
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.Extra.MonadGS          (MonadSscGS (..), sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Core        (GtPayload (..), VssCertificate (..),
                                                 VssCertificatesMap, checkCommShares,
                                                 checkShares, diffCommMap,
                                                 getCommitmentsMap, isCommitmentIdx,
                                                 isOpeningIdx, isSharesIdx, vcVssKey,
                                                 _gpCertificates)
import           Pos.Ssc.GodTossing.Error       (SeedError)
import           Pos.Ssc.GodTossing.Functions   (computeParticipants, getStableCertsPure,
                                                 verifyEntriesGuard, verifyGtPayload)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Seed        (calculateSeed)
import           Pos.Ssc.GodTossing.Toss        (TossVerErrorTag (..),
                                                 TossVerFailure (..),
                                                 checkOpeningMatchesCommitment)
import           Pos.Ssc.GodTossing.Type        (SscGodTossing)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..), gsCommitments,
                                                 gsOpenings, gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (Block, EpochIndex (..), EpochOrSlot (..),
                                                 SharedSeed, SlotId (..), addressHash,
                                                 blockMpc, blockSlot, epochIndexL,
                                                 epochOrSlot, epochOrSlotG, gbHeader)
import           Pos.Util                       (NE, NewestFirst (..), OldestFirst (..),
                                                 maybeThrow, toOldestFirst)

instance SscStorageClass SscGodTossing where
    sscLoadGlobalState = mpcLoadGlobalState
    sscApplyBlocksM = mpcApplyBlocks
    sscRollbackM = mpcRollback
    sscVerifyBlocksM pureVer rich = runExceptT . mpcVerifyBlocks pureVer rich
    sscCalculateSeedM = calculateSeedQ
