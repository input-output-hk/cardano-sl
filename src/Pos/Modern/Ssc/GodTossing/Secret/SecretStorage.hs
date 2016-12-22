{-# LANGUAGE FlexibleContexts #-}
module Pos.Modern.Ssc.GodTossing.Secret.SecretStorage
       (
         getSecret
       , setSecret
       , prepareSecretToNewSlot
       ) where

import           Universum

import           Pos.DB                                 (MonadDB)
import           Pos.DB.Misc                            (getSecretStorage,
                                                         putSecretStorage)
import           Pos.Modern.Ssc.GodTossing.Secret.Types (GtSecret, GtSecretStorage (..))
import           Pos.Ssc.GodTossing.Types.Type          (SscGodTossing)
import           Pos.Types                              (SlotId (..))

getSecret :: MonadDB SscGodTossing m => m (Maybe GtSecret)
getSecret = _dsCurrentSecret <$> getSecretStorage

setSecret :: MonadDB SscGodTossing m => GtSecret -> m ()
setSecret secret = do
    st <- getSecretStorage
    putSecretStorage $ st {_dsCurrentSecret = Just secret}

prepareSecretToNewSlot :: MonadDB SscGodTossing m => SlotId -> m ()
prepareSecretToNewSlot si@SlotId{siEpoch = epochIdx} = do
    st <- getSecretStorage
    if ((epochIdx >) . siEpoch $ _dsLastProcessedSlot st) then
        putSecretStorage $ GtSecretStorage Nothing si
    else
        putSecretStorage $ st {_dsLastProcessedSlot = si}
