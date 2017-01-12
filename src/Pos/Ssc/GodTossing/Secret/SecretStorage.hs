module Pos.Ssc.GodTossing.Secret.SecretStorage
       (
         getSecret
       , setSecret
       , getSecretForTip
       ) where

import           Universum

import           Pos.DB                          (MonadDB)
import           Pos.DB.Misc                     (getSecretStorage, putSecretStorage)
import           Pos.Ssc.GodTossing.Secret.Types (GtSecret, GtSecretStorage (..))
import           Pos.Ssc.GodTossing.Types.Type   (SscGodTossing)
import           Pos.Types                       (HeaderHash)

getSecret :: MonadDB SscGodTossing m => m (Maybe GtSecret)
getSecret = _dsCurrentSecret <$> getSecretStorage

getSecretForTip :: MonadDB SscGodTossing m => m (HeaderHash SscGodTossing)
getSecretForTip = _dsSecretForTip <$> getSecretStorage

setSecret :: MonadDB SscGodTossing m => GtSecret -> HeaderHash SscGodTossing -> m ()
setSecret secret tip = do
    st <- getSecretStorage
    putSecretStorage $ st {_dsCurrentSecret = Just secret, _dsSecretForTip = tip}
