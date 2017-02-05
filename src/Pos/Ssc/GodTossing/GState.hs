{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Instance of SscGStateClass.

module Pos.Ssc.GodTossing.GState
       ( -- * Instances
         -- ** instance SscGStateClass SscGodTossing
         getGlobalCerts
       , gtGetGlobalState
       , getStableCerts

       , computeCommitmentDistrById
       ) where

import           Control.Lens                   (at, (.=), _Wrapped)
import           Control.Monad.Except           (MonadError, runExceptT, throwError)
import           Data.Default                   (def)
import qualified Data.HashMap.Strict            as HM
import qualified Data.List.NonEmpty             as NE
import           Formatting                     (build, sformat, (%))
import           System.Wlog                    (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.Ssc                 ()
import           Pos.Constants                  (vssMaxTTL)
import           Pos.Context                    (WithNodeContext, lrcActionOnEpoch)
import           Pos.DB                         (DBError (DBMalformed), MonadDB,
                                                 getTipBlockHeader,
                                                 loadBlundsFromTipWhile)
import qualified Pos.DB.Lrc                     as LrcDB
import           Pos.Lrc.Types                  (RichmenStake)
import           Pos.Ssc.Class.Storage          (SscGStateClass (..), SscVerifier)
import           Pos.Ssc.Extra                  (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Core        (VssCertificatesMap)
import           Pos.Ssc.GodTossing.Functions   (getStableCertsPure)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Seed        (calculateSeed)
import           Pos.Ssc.GodTossing.Toss        (MultiRichmenStake, PureToss,
                                                 TossVerFailure (..), applyGenesisBlock,
                                                 computeCommitmentDistr, rollbackGT,
                                                 runPureTossWithLogger,
                                                 verifyAndApplyGtPayload)
import           Pos.Ssc.GodTossing.Type        (SscGodTossing)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..), gsCommitments,
                                                 gsOpenings, gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (Block, EpochIndex (..), SlotId (..),
                                                 StakeholderId, blockMpc, epochIndexL,
                                                 epochOrSlotG, gbHeader)
import           Pos.Util                       (NE, NewestFirst (..), OldestFirst (..),
                                                 maybeThrow, toOldestFirst, _neHead,
                                                 _neLast)

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

computeCommitmentDistrById
    :: (MonadError TossVerFailure m, MonadIO m,
        MonadDB SscGodTossing m, WithNodeContext SscGodTossing m)
    => EpochIndex
    -> StakeholderId
    -> m Word16
computeCommitmentDistrById epoch id = do
    richmen <- lrcActionOnEpoch epoch LrcDB.getRichmenSsc
    HM.lookupDefault 0 id <$> computeCommitmentDistr richmen
----------------------------------------------------------------------------
-- Methods from class
----------------------------------------------------------------------------

instance SscGStateClass SscGodTossing where
    sscLoadGlobalState = loadGlobalState
    sscRollbackU = rollbackBlocks
    sscVerifyAndApplyBlocks = verifyAndApply
    sscCalculateSeedQ _ =
        calculateSeed <$> view gsCommitments <*> view gsOpenings <*>
        view gsShares

loadGlobalState
    :: (WithNodeContext kek m, WithLogger m, MonadDB SscGodTossing m)
    => m GtGlobalState
loadGlobalState = do
    endEpoch <- view epochIndexL <$> getTipBlockHeader
    let startEpoch = safeSub endEpoch -- load blocks while >= endEpoch
        whileEpoch b = b ^. epochIndexL >= startEpoch
    logDebug $ sformat ("mpcLoadGlobalState: start epoch is " %build) startEpoch
    nfBlocks <- fmap fst <$> loadBlundsFromTipWhile whileEpoch
    blocks <-
        toOldestFirst <$>
        maybeThrow
            (DBMalformed "No blocks during mpc load global state")
            (_Wrapped NE.nonEmpty nfBlocks)
    let initGState
            | startEpoch == 0 = over gsVssCertificates unionGenCerts def
            | otherwise = def
    multiRichmen <- foldM loadRichmenStep mempty [startEpoch .. endEpoch]
    let action = verifyAndApplyMultiRichmen multiRichmen blocks
    let errMsg e = "invalid blocks, can't load GodTossing state: " <> pretty e
    runExceptT (execStateT action initGState) >>= \case
        Left e -> throwM $ DBMalformed $ errMsg e
        Right res ->
            res <$ (logInfo $ sformat ("Loaded GodTossing state: " %build) res)
  where
    safeSub epoch = epoch - min epoch vssMaxTTL
    unionGenCerts gs = foldr' VCD.insert gs . toList $ genesisCertificates
    loadRichmenStep resMap epoch =
        loadRichmenStepDo resMap epoch <$>
        lrcActionOnEpoch epoch LrcDB.getRichmenSsc
    loadRichmenStepDo resMap epoch richmen = resMap & at epoch .~ Just richmen

type GSUpdate a = forall m . (MonadState GtGlobalState m, WithLogger m) => m a

rollbackBlocks :: NewestFirst NE (Block SscGodTossing) -> GSUpdate ()
rollbackBlocks blocks = tossToUpdate mempty $ rollbackGT oldestEOS payloads
  where
    oldestEOS = blocks ^. _Wrapped . _neLast . epochOrSlotG
    payloads =
        NewestFirst . map (view blockMpc) . catMaybes . map hush . toList $
        blocks

verifyAndApply
    :: RichmenStake
    -> OldestFirst NE (Block SscGodTossing)
    -> SscVerifier SscGodTossing ()
verifyAndApply richmenSet blocks = verifyAndApplyMultiRichmen richmenData blocks
  where
    epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmenData = HM.fromList [(epoch, richmenSet)]

verifyAndApplyMultiRichmen
    :: MultiRichmenStake
    -> OldestFirst NE (Block SscGodTossing)
    -> SscVerifier SscGodTossing ()
verifyAndApplyMultiRichmen richmenData = mapM_ verifyAndApplyDo
  where
    verifyAndApplyDo (Left blk) =
        tossToVerifier richmenData $ applyGenesisBlock $ blk ^. epochIndexL
    verifyAndApplyDo (Right blk) =
        tossToVerifier richmenData $
        verifyAndApplyGtPayload (Right $ blk ^. gbHeader) (blk ^. blockMpc)

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
