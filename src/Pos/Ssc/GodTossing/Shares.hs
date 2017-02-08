{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Logic of working with Shares.

module Pos.Ssc.GodTossing.Shares
       ( getOurShares
       ) where

import           Crypto.Random            (drgNewSeed, seedNew, withDRG)
import qualified Data.HashMap.Strict      as HM
import           Formatting               (build, sformat, (%))
import           System.Wlog              (WithLogger, dispatchEvents, getLoggerName,
                                           logWarning, runPureLog, usingLoggerName)
import           Universum

import           Pos.Crypto               (EncShare, Share, VssKeyPair, VssPublicKey,
                                           decryptShare, toVssPublicKey)
import           Pos.Ssc.Class.Storage    (SscGlobalQuery)
import           Pos.Ssc.Extra            (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Core  (Commitment (..), getCommitmentsMap)
import           Pos.Ssc.GodTossing.Type  (SscGodTossing)
import           Pos.Ssc.GodTossing.Types (gsCommitments, gsOpenings)
import           Pos.Types.Address        (addressHash)
import           Pos.Types.Core           (StakeholderId)
import           Pos.Util                 (AsBinary, asBinary, fromBinaryM)

type GSQuery a = SscGlobalQuery SscGodTossing a

-- | Decrypt shares (in commitments) that are intended for us and that we can
-- decrypt.
getOurShares
    :: (MonadSscMem SscGodTossing m, MonadIO m, WithLogger m)
    => VssKeyPair -> m (HashMap StakeholderId (NonEmpty Share))
getOurShares ourKey = do
    randSeed <- liftIO seedNew
    let ourPK = asBinary $ toVssPublicKey ourKey
    encSharesM <- sscRunGlobalQuery (decryptOurShares ourPK)
    let drg = drgNewSeed randSeed
        (res, pLog) =
          fst . withDRG drg . runPureLog . usingLoggerName mempty <$>
          flip traverse (HM.toList encSharesM) $ \(id, lEncSh) -> do
              let mEncSh = traverse fromBinaryM lEncSh
              case mEncSh of
                Just encShares ->
                    lift . lift $ Just . (id,) <$> mapM (decryptShare ourKey) encShares
                _             -> do
                    logWarning $ sformat ("Failed to deserialize share for " % build) id
                    return Nothing
        resHM = HM.fromList . catMaybes $ res
    loggerName <- getLoggerName
    liftIO $ usingLoggerName loggerName $ dispatchEvents pLog
    return resHM

-- | Decrypt shares (in commitments) that we can decrypt.
decryptOurShares
    :: AsBinary VssPublicKey                           -- ^ Our VSS key
    -> GSQuery (HashMap StakeholderId (NonEmpty (AsBinary EncShare)))
decryptOurShares ourPK = do
    comms <- getCommitmentsMap <$> view gsCommitments
    opens <- view gsOpenings
    return . HM.fromList . catMaybes $ checkOpen opens <$> toList comms
  where
    checkOpen opens (addressHash -> theirId, Commitment {..}, _)
        | not $ HM.member theirId opens =
            (theirId,) <$> HM.lookup ourPK commShares
        | otherwise = Nothing -- if we have opening for theirAddr, we shouldn't send shares for it
