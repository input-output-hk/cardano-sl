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
       ) where

import           Control.Lens                   ((.=), _Wrapped)
import           Control.Monad.Except           (runExceptT, throwError)
import           Data.Default                   (def)
import qualified Data.List.NonEmpty             as NE
import           Formatting                     (build, sformat, (%))
import           System.Wlog                    (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.Ssc                 ()
import           Pos.Constants                  (vssMaxTTL)
import           Pos.DB                         (DBError (DBMalformed), MonadDB,
                                                 getTipBlockHeader,
                                                 loadBlundsFromTipWhile)
import           Pos.Lrc.Types                  (RichmenSet)
import           Pos.Ssc.Class.Storage          (SscGStateClass (..), SscVerifier)
import           Pos.Ssc.Extra                  (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Core        (VssCertificatesMap)
import           Pos.Ssc.GodTossing.Functions   (getStableCertsPure)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Seed        (calculateSeed)
import           Pos.Ssc.GodTossing.Toss        (PureToss, TossVerFailure (..),
                                                 applyGenesisBlock, rollbackGT,
                                                 runPureTossWithLogger,
                                                 verifyAndApplyGtPayload)
import           Pos.Ssc.GodTossing.Type        (SscGodTossing)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..), gsCommitments,
                                                 gsOpenings, gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (Block, EpochIndex (..), SlotId (..),
                                                 blockMpc, epochIndexL, epochOrSlot,
                                                 epochOrSlotG, gbHeader)
import           Pos.Util                       (NE, NewestFirst (..), OldestFirst (..),
                                                 maybeThrow, toOldestFirst, _neHead)

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
    sscRollbackU = rollbackBlocks
    sscVerifyAndApplyBlocks = verifyAndApply
    sscCalculateSeedQ _ =
        calculateSeed <$> view gsCommitments <*> view gsOpenings <*>
        view gsShares

loadGlobalState
    :: (WithLogger m, MonadDB SscGodTossing m)
    => m GtGlobalState
loadGlobalState = do
    tipBlockHeader <- getTipBlockHeader
    let endEpoch  = epochOrSlot identity siEpoch $ tipBlockHeader ^. epochOrSlotG
    let startEpoch = safeSub endEpoch -- load blocks while >= endEpoch
        whileEpoch b = b ^. epochIndexL >= startEpoch
    logDebug $ sformat ("mpcLoadGlobalState: start epoch is "%build) startEpoch
    nfBlocks <- fmap fst <$> loadBlundsFromTipWhile whileEpoch
    blocks <- toOldestFirst <$>
                  maybeThrow (DBMalformed "No blocks during mpc load global state")
                             (_Wrapped NE.nonEmpty nfBlocks)
    let initGState
            | startEpoch == 0 =
                over gsVssCertificates unionGenCerts def
            | otherwise = def
    -- res <- execStateT (mapM_ gtApplyBlock blocks) initGState
    res <- undefined
    res <$ (logInfo $ sformat ("Loaded GodTossing state: "%build) res)
  where
    safeSub epoch = epoch - min epoch vssMaxTTL
    unionGenCerts gs = foldr' VCD.insert gs . toList $ genesisCertificates

type GSUpdate a = forall m . (MonadState GtGlobalState m, WithLogger m) => m a

rollbackBlocks :: NewestFirst NE (Block SscGodTossing) -> GSUpdate ()
rollbackBlocks = mapM_ rollbackDo
  where
    dummyRichmen = (0, mempty) -- richmen shouldn't matter here
    rollbackDo :: Block SscGodTossing -> GSUpdate ()
    rollbackDo (Left _) = pass
    rollbackDo (Right (view blockMpc -> payload)) =
        tossToUpdate dummyRichmen $ rollbackGT payload

verifyAndApply
    :: RichmenSet
    -> OldestFirst NE (Block SscGodTossing)
    -> SscVerifier SscGodTossing ()
verifyAndApply richmenSet blocks = mapM_ verifyAndApplyDo blocks
  where
    epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmenData = (epoch, richmenSet)
    verifyAndApplyDo (Left _) =
        tossToVerifier richmenData $ applyGenesisBlock epoch
    verifyAndApplyDo (Right blk) =
        tossToVerifier richmenData $
        verifyAndApplyGtPayload (Right $ blk ^. gbHeader) (blk ^. blockMpc)

tossToUpdate :: (EpochIndex, RichmenSet) -> PureToss a -> GSUpdate a
tossToUpdate richmenData action = do
    oldState <- use identity
    (res, newState) <- runPureTossWithLogger richmenData oldState action
    (identity .= newState) $> res

tossToVerifier
    :: (EpochIndex, RichmenSet)
    -> ExceptT TossVerFailure PureToss a
    -> SscVerifier SscGodTossing a
tossToVerifier richmenData action = do
    oldState <- use identity
    (resOrErr, newState) <-
        runPureTossWithLogger richmenData oldState $ runExceptT action
    case resOrErr of
        Left e    -> throwError e
        Right res -> (identity .= newState) $> res
