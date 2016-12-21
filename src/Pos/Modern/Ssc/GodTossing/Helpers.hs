{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Modern.Ssc.GodTossing.Helpers
       ( calculateSeedQ
       , getOurShares
       ) where
import           Control.Lens                            (view)
import           Crypto.Random                           (drgNewSeed, seedNew, withDRG)
import qualified Data.HashMap.Strict                     as HM
import           Data.List                               (nub)
import           Data.List.NonEmpty                      (NonEmpty)
import qualified Data.List.NonEmpty                      as NE
import           Formatting                              (build, sformat, (%))
import           System.Wlog                             (HasLoggerName, dispatchEvents,
                                                          getLoggerName, logWarning,
                                                          runPureLog, usingLoggerName)
import           Universum

import           Pos.Crypto                              (EncShare, Share, Threshold,
                                                          VssKeyPair, VssPublicKey,
                                                          decryptShare, toVssPublicKey)
import           Pos.Ssc.Class.Storage                   (SscGlobalQueryM)
import           Pos.Ssc.Class.Types                     (Ssc (..))
import           Pos.Ssc.Extra.MonadGS                   (MonadSscGS, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Error                (SeedError)
import           Pos.Ssc.GodTossing.Seed                 (calculateSeed)
import           Pos.Ssc.GodTossing.Types.Base           (Commitment (..),
                                                          VssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Type           (SscGodTossing)
import           Pos.Types                               (Address (..), EpochIndex,
                                                          SharedSeed, Utxo, txOutAddress)
import           Pos.Util                                (AsBinary, asBinary, fromBinaryM)

import           Pos.Modern.Ssc.GodTossing.Storage.Types (GtGlobalState (..),
                                                          gsCommitments, gsOpenings,
                                                          gsShares, gsVssCertificates)

type GSQuery a = SscGlobalQueryM SscGodTossing a

-- | Get keys of nodes participating in an epoch. A node participates if,
-- when there were 'k' slots left before the end of the previous epoch, both
-- of these were true:
--
--   1. It was a stakeholder.
--   2. It had already sent us its VSS key by that time.
getParticipants
    :: (SscGlobalStateM SscGodTossing ~ GtGlobalState)
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

-- | Calculate leaders for the next epoch.
calculateSeedQ
    :: (SscGlobalStateM SscGodTossing ~ GtGlobalState)
    => EpochIndex
    -> Threshold
    -> GSQuery (Either SeedError SharedSeed)
calculateSeedQ _ threshold = do --GodTossing doesn't use epoch, but NistBeacon use it
    calculateSeed threshold
                  <$> view gsCommitments
                  <*> view gsOpenings
                  <*> view gsShares

----------------------------------------------------------------------------
-- Worker Helper
----------------------------------------------------------------------------

-- | Decrypt shares (in commitments) that are intended for us and that we can
-- decrypt.
getOurShares :: ( MonadSscGS SscGodTossing m
                , MonadIO m, HasLoggerName m
                , SscGlobalStateM SscGodTossing ~ GtGlobalState)
             => VssKeyPair -> m (HashMap Address Share)
getOurShares ourKey = do
    randSeed <- liftIO seedNew
    let ourPK = asBinary $ toVssPublicKey ourKey
    encSharesM <- sscRunGlobalQuery (decryptOurShares ourPK)
    let drg = drgNewSeed randSeed
        (res, pLog) =
          fst . withDRG drg . runPureLog . usingLoggerName mempty <$>
          flip traverse (HM.toList encSharesM) $ \(pk, lEncSh) -> do
              let mEncSh = fromBinaryM lEncSh
              case mEncSh of
                Just encShare ->
                    lift . lift $ Just . (,) pk <$> decryptShare ourKey encShare
                _             -> do
                    logWarning $ sformat ("Failed to deserialize share for " % build) pk
                    return Nothing
        resHM = HM.fromList . catMaybes $ res
    loggerName <- getLoggerName
    liftIO $ usingLoggerName loggerName $ dispatchEvents pLog
    return resHM

-- | Decrypt shares (in commitments) that we can decrypt.
decryptOurShares
    :: (SscGlobalStateM SscGodTossing ~ GtGlobalState)
    => AsBinary VssPublicKey                           -- ^ Our VSS key
    -> GSQuery (HashMap Address (AsBinary EncShare))
decryptOurShares ourPK = do
    comms <- view gsCommitments
    opens <- view gsOpenings
    return .
        HM.fromList . catMaybes $
            flip fmap (HM.toList comms) $ \(theirAddr, (_, Commitment{..}, _)) ->
                if not $ HM.member theirAddr opens
                   then (,) theirAddr <$> HM.lookup ourPK commShares
                   else Nothing -- if we have opening for theirAddr, we shouldn't send shares for it
