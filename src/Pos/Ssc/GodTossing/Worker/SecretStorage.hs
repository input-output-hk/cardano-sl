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
       , checkpointSecret
       , bracket'
       , SecretStorage
       ) where

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

type SecretStorage  = ExtendedState GtSecretStorage

bracket' :: (MonadMask m, MonadIO m) => Maybe FilePath -> (SecretStorage -> m a) -> m a
bracket' pathToSecret call =
    bracket (maybe
                 openMemGtSecretState
                 (openGtSecretState False)
                 pathToSecret)
             closeExtendedState
             call

getSecret :: MonadIO m => SecretStorage -> m (Maybe GtSecret)
getSecret = flip queryExtended GetS

setSecret :: MonadIO m => SecretStorage -> GtSecret -> m ()
setSecret storage secret = updateExtended storage (SetS secret)

prepareSecretToNewSlot :: MonadIO m => SecretStorage -> SlotId -> m ()
prepareSecretToNewSlot storage slotId =
    updateExtended storage (Prepare slotId)

checkpointSecret :: MonadIO m => SecretStorage -> m ()
checkpointSecret = tidyExtendedState
