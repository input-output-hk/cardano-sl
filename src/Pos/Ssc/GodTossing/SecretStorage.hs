module Pos.Ssc.GodTossing.SecretStorage
       (
         getSecret
       , getSecretNEpoch
       , getSecretNTip
       , setSecret
       ) where

import           Universum

import           Pos.DB                         (MonadDB)
import           Pos.DB.Misc                    (getSecretStorage, putSecretStorage)
import           Pos.Ssc.GodTossing.Types.Type  (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types (GtSecret, GtSecretStorage (..))
import           Pos.Types                      (EpochIndex, HeaderHash)

getSecret :: MonadDB SscGodTossing m => m (Maybe GtSecret)
getSecret = do
    mb <- _dsCurrentSecret <$> getSecretStorage
    pure $ maybe Nothing (\(s, _, _) -> Just s) mb

getSecretNEpoch :: MonadDB SscGodTossing m => m (Maybe (GtSecret, EpochIndex))
getSecretNEpoch = do
    mb <- _dsCurrentSecret <$> getSecretStorage
    pure $ maybe Nothing (\(s, e, _) -> Just (s, e)) mb

getSecretNTip :: MonadDB SscGodTossing m => m (Maybe (GtSecret, HeaderHash SscGodTossing))
getSecretNTip = do
    mb <- _dsCurrentSecret <$> getSecretStorage
    pure $ maybe Nothing (\(s, _, t) -> Just (s, t)) mb

setSecret :: MonadDB SscGodTossing m
          => GtSecret
          -> EpochIndex
          -> HeaderHash SscGodTossing
          -> m ()
setSecret secret epoch tip = putSecretStorage . GtSecretStorage . Just $ (secret, epoch, tip)
