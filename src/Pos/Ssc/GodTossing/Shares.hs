{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Logic of working with Shares.

module Pos.Ssc.GodTossing.Shares
       (
         getOurShares
       ) where

import           Control.Lens             (view)
import           Crypto.Random            (drgNewSeed, seedNew, withDRG)
import qualified Data.HashMap.Strict      as HM
import           Formatting               (build, sformat, (%))
import           System.Wlog              (HasLoggerName, dispatchEvents, getLoggerName,
                                           logWarning, runPureLog, usingLoggerName)
import           Universum

import           Pos.Crypto               (EncShare, PublicKey, Share, VssKeyPair,
                                           VssPublicKey, decryptShare, toVssPublicKey)
import           Pos.Ssc.Class.Storage    (SscGlobalQuery)
import           Pos.Ssc.Extra.MonadGS    (MonadSscGS, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Types (Commitment (..), SscGodTossing, gsCommitments,
                                           gsOpenings)
import           Pos.Types                (AddressHash)
import           Pos.Util                 (AsBinary, asBinary, fromBinaryM)

type GSQuery a = SscGlobalQuery SscGodTossing a

-- | Decrypt shares (in commitments) that are intended for us and that we can
-- decrypt.
getOurShares :: ( MonadSscGS SscGodTossing m
                , MonadIO m, HasLoggerName m)
             => VssKeyPair -> m (HashMap (AddressHash PublicKey) Share)
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
    :: AsBinary VssPublicKey                           -- ^ Our VSS key
    -> GSQuery (HashMap (AddressHash PublicKey) (AsBinary EncShare))
decryptOurShares ourPK = do
    comms <- view gsCommitments
    opens <- view gsOpenings
    return .
        HM.fromList . catMaybes $
            flip fmap (HM.toList comms) $ \(theirAddr, (_, Commitment{..}, _)) ->
                if not $ HM.member theirAddr opens
                   then (,) theirAddr <$> HM.lookup ourPK commShares
                   else Nothing -- if we have opening for theirAddr, we shouldn't send shares for it
