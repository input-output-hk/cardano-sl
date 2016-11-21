{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Pos.Ssc.GodTossing.Worker.SecretStorage
       (
         getSecret
       , setSecret
       , prepareSecretToNewSlot
       , checkpoint
       ) where
--import           Control.Lens               (Lens', use, view, (.=))
import           Control.Monad.State             (get, put)
import           Data.Acid                       (Query, Update, makeAcidic)
import           Data.Default                    (def)
import           Pos.Ssc.GodTossing.Worker.Types (GtSecret, GtSecretStorage (..))
import           Pos.Types                       (SlotId (..))
import           Serokell.AcidState              (ExtendedState)
import           Serokell.AcidState              (closeExtendedState,
                                                  openLocalExtendedState,
                                                  openMemoryExtendedState, queryExtended,
                                                  tidyExtendedState, updateExtended)
import           Universum

getS :: Query GtSecretStorage (Maybe GtSecret)
getS = asks _dsCurrentSecret

setS :: GtSecret -> Update GtSecretStorage ()
setS (ourPk, comm, op) = do
    st <- get
    case _dsCurrentSecret st of
        Just _  -> panic "setSecret: a secret was already present"
        Nothing -> put $ st {_dsCurrentSecret = Just (ourPk, comm, op)}

prepare :: SlotId -> Update GtSecretStorage ()
prepare si@SlotId {siEpoch = epochIdx} = do
    st <- get
    if ((epochIdx >) . siEpoch $ _dsLastProcessedSlot st) then
        put $ GtSecretStorage Nothing si
    else
        put $ st {_dsLastProcessedSlot = si}
----------------------------------------------------------------------------
-- Acidic Secret Storage
----------------------------------------------------------------------------
openGtSecretState :: MonadIO m
                  => Bool
                  -> FilePath
                  -> m (ExtendedState GtSecretStorage)
openGtSecretState deleteIfExists fp =
    openLocalExtendedState deleteIfExists fp def


openMemGtSecretState :: MonadIO m
             => m (ExtendedState GtSecretStorage)
openMemGtSecretState = openMemoryExtendedState def

makeAcidic ''GtSecretStorage
    [
      'getS
    , 'setS
    , 'prepare
    ]

bracket' :: (MonadMask m, MonadIO m) => Maybe FilePath -> (ExtendedState GtSecretStorage -> m a) -> m a
bracket' pathToSecret call =
    bracket (maybe
                 openMemGtSecretState
                 (openGtSecretState False)
                 pathToSecret)
             closeExtendedState
             call

getSecret :: (MonadMask m, MonadIO m) => Maybe FilePath -> m (Maybe GtSecret)
getSecret pathToSecret = bracket' pathToSecret $ flip queryExtended GetS

setSecret :: (MonadMask m, MonadIO m) => Maybe FilePath -> GtSecret -> m ()
setSecret pathToSecret secret = bracket' pathToSecret $
    flip updateExtended (SetS secret)

prepareSecretToNewSlot :: (MonadMask m, MonadIO m) => Maybe FilePath -> SlotId -> m ()
prepareSecretToNewSlot pathToSecret slotId = bracket' pathToSecret $
    flip updateExtended (Prepare slotId)

checkpoint :: (MonadMask m, MonadIO m) => Maybe FilePath -> m ()
checkpoint pathToSecret = bracket' pathToSecret $ tidyExtendedState
