{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

module Pos.Ssc.GodTossing.Instance.SecretStorage
       (
           -- Instances here
       ) where
import           Control.Lens               (Lens', use, view, (.=))
import           Pos.Ssc.GodTossing.Storage (GtSecret, GtSecretStorage,
                                             GtSecretStorageClass (..),
                                             HasGtSecretStorage (..), SecQuery, SecUpdate,
                                             dsCurrentSecretL, dsSecLastProcessedSlotL)
import           Pos.Types                  (SlotId (..))
import           Universum

instance HasGtSecretStorage GtSecretStorage GtSecretStorage where
    secretStorage = identity

dsCurrentSecret
    :: HasGtSecretStorage GtSecretStorage a =>
       Lens' a (Maybe GtSecret)
dsCurrentSecret = secretStorage @GtSecretStorage . dsCurrentSecretL

dsSecLastProcessedSlot
    :: HasGtSecretStorage GtSecretStorage a =>
       Lens' a SlotId
dsSecLastProcessedSlot = secretStorage @GtSecretStorage . dsSecLastProcessedSlotL

instance GtSecretStorageClass GtSecretStorage where
    ssGetSecret = getSecret
    ssSetSecret = setSecret
    ssPrepareToNewSlot = prepare

getSecret :: SecQuery (Maybe GtSecret)
getSecret = view dsCurrentSecret

setSecret :: GtSecret -> SecUpdate ()
setSecret (ourPk, comm, op) = do
    s <- use dsCurrentSecret
    case s of
        Just _  -> panic "setSecret: a secret was already present"
        Nothing -> dsCurrentSecret .= Just (ourPk, comm, op)

prepare :: SlotId -> SecUpdate ()
prepare si@SlotId {siEpoch = epochIdx} = do
    whenM ((epochIdx >) . siEpoch <$> use dsSecLastProcessedSlot) $
        dsCurrentSecret .= Nothing
    dsSecLastProcessedSlot .= si
