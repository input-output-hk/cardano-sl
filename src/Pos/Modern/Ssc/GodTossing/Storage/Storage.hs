{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Instance of SscStorageClass.

module Pos.Modern.Ssc.GodTossing.Storage.Storage
       ( -- * Instances
         -- ** instance SscStorageClass SscGodTossing
         getGlobalCertificates
       ) where

import           Control.Lens                            (use, view, (%=), (.=), (^.), _1,
                                                          _2)
import           Control.Monad.IfElse                    (whileM)
import           Control.Monad.Reader                    (ask)
import           Data.Default                            (def)
import qualified Data.HashMap.Strict                     as HM
import           Serokell.Util.Verify                    (VerificationRes (..),
                                                          isVerSuccess, verifyGeneric)
import           Universum

import           Pos.Binary.Ssc                          ()
import           Pos.DB                                  (MonadDB, getBlock)
import           Pos.Modern.Ssc.GodTossing.Helpers       (calculateSeedQ)
import           Pos.Modern.Ssc.GodTossing.Storage.Types (GtGlobalState (..),
                                                          gsCommitments, gsOpenings,
                                                          gsShares, gsVssCertificates)
import           Pos.Ssc.Class.Storage                   (SscStorageClassM (..))
import           Pos.Ssc.Class.Types                     (Ssc (..))
import           Pos.Ssc.Extra.MonadGS                   (MonadSscGS (..),
                                                          sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Functions            (checkOpeningMatchesCommitment,
                                                          checkShares, isCommitmentIdx,
                                                          isOpeningIdx, isSharesIdx,
                                                          verifyGtPayload)
import           Pos.Ssc.GodTossing.Types                (GtPayload (..), SscGodTossing,
                                                          VssCertificatesMap,
                                                          _gpCertificates)
import           Pos.State.Storage.Types                 (AltChain)
import           Pos.Types                               (Block, HeaderHash, SlotId (..),
                                                          blockMpc, blockSlot, gbHeader,
                                                          prevBlockL)
import           Pos.Util                                (readerToState)

type GSQuery a  = forall m . (MonadReader GtGlobalState m) => m a
type GSUpdate a = forall m . (MonadState GtGlobalState m) => m a

instance SscStorageClassM SscGodTossing where
    sscLoadGlobalState = mpcLoadGlobalState
    sscApplyBlocksM = mpcApplyBlocks
    sscRollbackM = mpcRollback
    sscVerifyBlocksM = mpcVerifyBlocks
    sscCalculateSeedM = calculateSeedQ

getGlobalCertificates
    :: (MonadSscGS SscGodTossing m)
    => m VssCertificatesMap
getGlobalCertificates = sscRunGlobalQuery $ view gsVssCertificates

-- | Verify that if one adds given block to the current chain, it will
-- remain consistent with respect to SSC-related data.
mpcVerifyBlock :: Block SscGodTossing -> GSQuery VerificationRes
-- Genesis blocks don't have any SSC data.
mpcVerifyBlock (Left _) = return VerSuccess
-- Main blocks have commitments, openings, shares and VSS certificates.
-- We use verifyGtPayload to make the most general checks and also use
-- global data to make more checks using this data.
mpcVerifyBlock (Right b) = do
    let SlotId{siSlot = slotId} = b ^. blockSlot
    let payload      = b ^. blockMpc

    globalCommitments  <- view gsCommitments
    globalOpenings     <- view gsOpenings
    globalShares       <- view gsShares
    globalCertificates <- view gsVssCertificates

    let isComm       = (isCommitmentIdx slotId, "slotId doesn't belong commitment phase")
        isOpen       = (isOpeningIdx slotId, "slotId doesn't belong openings phase")
        isShare      = (isSharesIdx slotId, "slotId doesn't belong share phase")
    -- For commitments we
    --   * check that the nodes haven't already sent their commitments before
    --     in some different block
    --   * check that a VSS certificate is present for the committing node
    let commChecks comms certs =
            [ isComm
            , (all (`HM.member` (certs <> globalCertificates))
                   (HM.keys comms),
                   "some committing nodes haven't sent a VSS certificate")
            , (all (not . (`HM.member` globalCommitments))
                   (HM.keys comms),
                   "some nodes have already sent their commitments")
            -- [CSL-206]: check that share IDs are different.
            ]

    -- For openings, we check that
    --   * the opening isn't present in previous blocks
    --   * corresponding commitment is present
    --   * the opening matches the commitment
    let openChecks opens =
            [ isOpen
            , (all (not . (`HM.member` globalOpenings))
                   (HM.keys opens),
                   "some nodes have already sent their openings")
            , (all (`HM.member` globalCommitments)
                   (HM.keys opens),
                   "some openings don't have corresponding commitments")
            , (all (checkOpeningMatchesCommitment globalCommitments) (HM.toList opens),
                   "some openings don't match corresponding commitments")
            ]

    -- For shares, we check that
    --   * shares have corresponding commitments
    --   * these shares weren't sent before
    --   * if encrypted shares (in commitments) are decrypted, they match
    --     decrypted shares
    -- We don't check whether shares match the openings.
    let shareChecks shares =
            [ isShare
            --We intentionally don't check, that nodes which decrypted shares
            --sent its commitments.
            --If node decrypted shares correctly, such node is useful for us, despite of
            --it didn't send its commitment.
            , (all (`HM.member` globalCommitments)
                   (concatMap HM.keys (toList shares)),
                   "some shares don't have corresponding commitments")
            -- [CSL-203]: here we assume that all shares are always sent as a
            -- whole package.
            -- Use intersectionDoubleMap (doesn't exist yet) or something to
            -- allow spliting shares into multiple messages.
            , (null (shares `HM.intersection` globalShares),
                   "some shares have already been sent")
            , (all (uncurry (checkShares globalCommitments globalOpenings
                             globalCertificates)) $
                     HM.toList shares,
                   "some decrypted shares don't match encrypted shares \
                   \in the corresponding commitment")
            ]

    let ourRes = verifyGeneric $
            case payload of
                CommitmentsPayload comms certs -> commChecks comms certs
                OpeningsPayload        opens _ -> openChecks opens
                SharesPayload         shares _ -> shareChecks shares
                CertificatesPayload          _ -> []
    return (verifyGtPayload (b ^. gbHeader) payload <> ourRes)

-- TODO:
--   ★ verification messages should include block hash/slotId
--   ★ we should stop at first failing block
mpcVerifyBlocks :: AltChain SscGodTossing -> GSQuery VerificationRes
mpcVerifyBlocks  blocks = do
    curState <- ask
    return $ flip evalState curState $ do
        vs <- forM blocks $ \b -> do
            v <- readerToState $ mpcVerifyBlock b
            when (isVerSuccess v) $
                mpcProcessBlock b
            return v
        return (fold vs)

unionPayload :: GtPayload -> GtGlobalState -> GtGlobalState
unionPayload payload =
    execState (do
    let blockCertificates = _gpCertificates payload
    case payload of
        CommitmentsPayload comms _ ->
            gsCommitments %= HM.union comms
        OpeningsPayload    opens _ ->
            gsOpenings %= HM.union opens
        SharesPayload     shares _ ->
            gsShares %= HM.unionWith HM.union shares
        CertificatesPayload      _ -> pure ()
    gsVssCertificates %= HM.union blockCertificates)

-- | Apply sequence of blocks to state. Sequence must be based on last
-- applied block and must be valid.
mpcApplyBlocks :: AltChain SscGodTossing -> GSUpdate ()
mpcApplyBlocks = mapM_ mpcProcessBlock

mpcProcessBlock
    :: (SscPayload ssc ~ GtPayload)
    => Block ssc -> GSUpdate ()
mpcProcessBlock blk = do
    case blk of
        -- Genesis blocks don't contain anything interesting, but when they
        -- “arrive”, we clear global commitments and other globals. Not
        -- certificates, though, because we don't want to make nodes resend
        -- them in each epoch.
        Left _  -> resetGS
        -- Main blocks contain commitments, openings, shares, VSS certificates
        Right b -> modify (unionPayload (b ^. blockMpc))

resetGS :: GSUpdate ()
resetGS = do
    gsCommitments .= mempty
    gsOpenings    .= mempty
    gsShares      .= mempty

-- | Head - younges
mpcRollback :: AltChain SscGodTossing -> GSUpdate ()
mpcRollback blocks = do
    wasGenesis <- foldM (\wasGen b -> if wasGen then pure wasGen else differenceBlock b)
                         False blocks
    when wasGenesis resetGS
  where
    differenceBlock :: Block SscGodTossing -> GSUpdate Bool
    differenceBlock (Left _)  = pure True
    differenceBlock (Right b) = do
        let payload = b ^. blockMpc
            payloadCertificates = _gpCertificates payload
        -- Gromak, don't beat me please, I tried to use zoom, but poslan
        case payload of
            CommitmentsPayload comms _ ->
                gsCommitments %= (`HM.difference` comms) -- ugly
            OpeningsPayload    opens _ ->
                gsOpenings %= (`HM.difference` opens)
            SharesPayload     shares _ ->
                gsShares %= (`HM.difference` shares)
            CertificatesPayload      _ -> return ()
        gsVssCertificates %= (`HM.difference` payloadCertificates)
        pure False

-- TODO union of certificats is invalid
-- | Union payloads of blocks until meet genesis block
unionBlocks :: MonadDB SscGodTossing m => StateT (GtGlobalState, HeaderHash SscGodTossing) m ()
unionBlocks = whileM
    (do
        curTip <- use _2
        block <- lift $ getBlock curTip
        maybe (panic "No block with such tip")
              (\b->
              case b of
                  Left _   -> pure False
                  Right mb -> do
                      _1 %= unionPayload (mb ^. blockMpc)
                      True <$ (_2 .= b ^. prevBlockL)
              )
              block
    ) (pure ())

mpcLoadGlobalState :: MonadDB SscGodTossing m => HeaderHash SscGodTossing -> m GtGlobalState
mpcLoadGlobalState tip = fst <$> execStateT unionBlocks (def, tip)
