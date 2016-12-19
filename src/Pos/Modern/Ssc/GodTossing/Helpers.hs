{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Modern.Ssc.GodTossing.Helpers
       (
       ) where
import           Control.Lens                            (view)
import qualified Data.HashMap.Strict                     as HM
import           Data.List                               (nub)
import           Data.List.NonEmpty                      (NonEmpty)
import qualified Data.List.NonEmpty                      as NE
import           Universum

import           Pos.Crypto                              (EncShare, Threshold,
                                                          VssPublicKey)
import           Pos.FollowTheSatoshi                    (followTheSatoshi)
import           Pos.Ssc.Class.Types                     (Ssc (..))
import           Pos.Ssc.GodTossing.Error                (SeedError)
import           Pos.Ssc.GodTossing.Seed                 (calculateSeed)
import           Pos.Ssc.GodTossing.Types.Base           (Commitment (..),
                                                          VssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Type           (SscGodTossing)
import           Pos.Types                               (Address (..), EpochIndex,
                                                          SlotLeaders, Utxo, txOutAddress)
import           Pos.Util                                (AsBinary)

import           Pos.Modern.Ssc.GodTossing.Storage.Types (GtGlobalState (..),
                                                          gsCommitments, gsOpenings,
                                                          gsShares, gsVssCertificates)
import           Pos.Ssc.Class.Helpers                   (SscHelpersClassM (..), SscQuery)

type GSQuery a = SscQuery SscGodTossing a

instance (Ssc SscGodTossing
         , SscGlobalState SscGodTossing ~ GtGlobalState
         , SscSeedError SscGodTossing ~ SeedError)
         => SscHelpersClassM SscGodTossing where
    sscGetOurSharesQ = getOurShares
    sscGetParticipantsQ = getParticipants
    sscCalculateLeadersQ = calculateLeaders

-- | Get keys of nodes participating in an epoch. A node participates if,
-- when there were 'k' slots left before the end of the previous epoch, both
-- of these were true:
--
--   1. It was a stakeholder.
--   2. It had already sent us its VSS key by that time.
getParticipants
    :: (SscGlobalState SscGodTossing ~ GtGlobalState)
    => Word -> Utxo -> GSQuery (Maybe (NonEmpty (AsBinary VssPublicKey)))
getParticipants depth utxo = do
    mKeymap <- Just <$> view gsVssCertificates
    --mKeymap <- fmap _gsVssCertificates <$> getGlobalMpcDataByDepth depth :
    -- is it right? or we don't care about verified certs
    return $
        do keymap <- mKeymap
           let stakeholders =
                   nub $ map txOutAddress (toList utxo)
           NE.nonEmpty $
               map vcVssKey $ mapMaybe (`HM.lookup` keymap) stakeholders

-- | Decrypt shares (in commitments) that we can decrypt.
getOurShares
    :: (SscGlobalState SscGodTossing ~ GtGlobalState)
    => AsBinary VssPublicKey                           -- ^ Our VSS key
    -> GSQuery (HashMap Address (AsBinary EncShare))
getOurShares ourPK = do
    comms <- view gsCommitments
    opens <- view gsOpenings
    return .
        HM.fromList . catMaybes $
            flip fmap (HM.toList comms) $ \(theirAddr, (_, Commitment{..}, _)) ->
                if not $ HM.member theirAddr opens
                   then (,) theirAddr <$> HM.lookup ourPK commShares
                   else Nothing -- if we have opening for theirAddr, we shouldn't send shares for it

-- | Calculate leaders for the next epoch.
calculateLeaders
    :: (SscGlobalState SscGodTossing ~ GtGlobalState)
    => EpochIndex
    -> Utxo            -- ^ Utxo (k slots before the end of epoch)
    -> Threshold
    -> GSQuery (Either SeedError SlotLeaders)
calculateLeaders _ utxo threshold = do --GodTossing doesn't use epoch, but NistBeacon use it
    mbSeed <- calculateSeed
                  threshold
                  <$> view gsCommitments
                  <*> view gsOpenings
                  <*> view gsShares
    return $ case mbSeed of
        Left e     -> Left e
        Right seed -> Right $ followTheSatoshi seed utxo
