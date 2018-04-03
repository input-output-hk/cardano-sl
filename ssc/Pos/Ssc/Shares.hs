{-# LANGUAGE RankNTypes #-}

-- | Logic of working with Shares.

module Pos.Ssc.Shares
       ( getOurShares
       ) where

import           Universum hiding (id)

import           Crypto.Random (drgNewSeed, seedNew, withDRG)
import qualified Data.HashMap.Strict as HM
import           Formatting (build, sformat, (%))
import           System.Wlog (WithLogger, launchNamedPureLog, logWarning)

import           Pos.Binary.Class (AsBinary, asBinary, fromBinary)
import           Pos.Core.Common (StakeholderId, addressHash)
import           Pos.Core.Ssc (Commitment (..), getCommitmentsMap)
import           Pos.Crypto (DecShare, EncShare, VssKeyPair, VssPublicKey, decryptShare,
                             toVssPublicKey)
import           Pos.Ssc.Mem (MonadSscMem, SscGlobalQuery, sscRunGlobalQuery)
import           Pos.Ssc.Types (sgsCommitments, sgsOpenings)

-- | Decrypt shares (in commitments) that are intended for us and that we can
-- decrypt.
getOurShares
    :: (MonadSscMem ctx m, MonadIO m, WithLogger m)
    => VssKeyPair -> m (HashMap StakeholderId (NonEmpty DecShare))
getOurShares ourKey = do
    randSeed <- liftIO seedNew
    let ourPK = asBinary $ toVssPublicKey ourKey
    encSharesM <- sscRunGlobalQuery (decryptOurShares ourPK)
    let drg = drgNewSeed randSeed
    res <- launchNamedPureLog (return . fst . withDRG drg) $
           forM (HM.toList encSharesM) $ \(id, lEncSh) -> do
              let mEncSh = traverse (rightToMaybe . fromBinary) lEncSh
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
    -> SscGlobalQuery (HashMap StakeholderId (NonEmpty (AsBinary EncShare)))
decryptOurShares ourPK = do
    comms <- getCommitmentsMap <$> view sgsCommitments
    opens <- view sgsOpenings
    return . HM.fromList . catMaybes $ checkOpen opens <$> toList comms
  where
    checkOpen opens (addressHash -> theirId, Commitment {..}, _)
        | not $ HM.member theirId opens =
            (theirId,) <$> HM.lookup ourPK commShares
        | otherwise = Nothing -- if we have opening for theirAddr, we shouldn't send shares for it
