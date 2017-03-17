{-# LANGUAGE RankNTypes #-}

-- | Logic of working with Shares.

module Pos.Ssc.GodTossing.Shares
       ( getOurShares
       ) where

import           Crypto.Random            (drgNewSeed, seedNew, withDRG)
import qualified Data.HashMap.Strict      as HM
import           Formatting               (build, sformat, (%))
import           System.Wlog              (WithLogger, launchNamedPureLog, logWarning)
import           Universum

import           Pos.Binary.Class         (AsBinary, asBinary, fromBinaryM)
import           Pos.Core.Address         (addressHash)
import           Pos.Core.Types           (StakeholderId)
import           Pos.Crypto               (EncShare, Share, VssKeyPair, VssPublicKey,
                                           decryptShare, toVssPublicKey)
import           Pos.Ssc.Class.Storage    (SscGlobalQuery)
import           Pos.Ssc.Extra            (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Core  (Commitment (..), getCommitmentsMap)
import           Pos.Ssc.GodTossing.Type  (SscGodTossing)
import           Pos.Ssc.GodTossing.Types (gsCommitments, gsOpenings)

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
    res <- launchNamedPureLog (return . fst . withDRG drg) $
           forM (HM.toList encSharesM) $ \(id, lEncSh) -> do
              let mEncSh = traverse fromBinaryM lEncSh
              case mEncSh of
                Just encShares ->
                    lift $ Just . (id,) <$> mapM (decryptShare ourKey) encShares
                _             -> do
                    logWarning $ sformat ("Failed to deserialize share for " % build) id
                    return Nothing
    return $ HM.fromList . catMaybes $ res

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
