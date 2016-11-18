{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

module Pos.Ssc.GodTossing.Instance.AcidicSecret
       (
         getSecret
       , setSecret
       , prepareSecretToNewSlot
       ) where


import           Data.Acid                                 (EventResult, EventState,
                                                            QueryEvent, UpdateEvent,
                                                            makeAcidicWithHacks)
import           Data.Default                              (def)
import           Pos.Ssc.GodTossing.Instance.SecretStorage ()
import           Pos.Ssc.GodTossing.Storage
import           Pos.Types                                 (SlotId)
import           Serokell.AcidState                        (ExtendedState,
                                                            closeExtendedState,
                                                            openLocalExtendedState,
                                                            openMemoryExtendedState,
                                                            queryExtended, updateExtended)
import           System.FilePath                           (FilePath)
import           Universum

openGtSecretState :: MonadIO m
                  => Bool
                  -> FilePath
                  -> m (ExtendedState GtSecretStorage)
openGtSecretState deleteIfExists fp =
    openLocalExtendedState deleteIfExists fp def


openMemGtSecretState :: MonadIO m
             => m (ExtendedState GtSecretStorage)
openMemGtSecretState = openMemoryExtendedState def

queryGtSecret
    :: (EventState event ~ GtSecretStorage,
        QueryEvent event, MonadIO m)
    => ExtendedState GtSecretStorage -> event -> m (EventResult event)
queryGtSecret = queryExtended

updateGtSecret
    :: (EventState event ~ GtSecretStorage,
        UpdateEvent event, MonadIO m)
    => ExtendedState GtSecretStorage -> event -> m (EventResult event)
updateGtSecret = updateExtended


type SecQuery'  a = forall m . MonadReader GtSecretStorage m => m a
type SecUpdate' a = forall m . MonadState GtSecretStorage m => m a

getSecret_ :: SecQuery' (Maybe GtSecret)
getSecret_ = ssGetSecret @GtSecretStorage

setSecret_ :: GtSecret -> SecUpdate' ()
setSecret_ = ssSetSecret @GtSecretStorage

prepareSecretToNewSlot_ :: SlotId -> SecUpdate' ()
prepareSecretToNewSlot_ = ssPrepareToNewSlot @GtSecretStorage


makeAcidicWithHacks ''GtSecretStorage []
    [
      'getSecret_
    , 'setSecret_
    , 'prepareSecretToNewSlot_
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
getSecret pathToSecret = bracket' pathToSecret $ \storage -> do
    secret <- queryGtSecret storage GetSecret_
    return secret

setSecret :: (MonadMask m, MonadIO m) => Maybe FilePath -> GtSecret -> m ()
setSecret pathToSecret secret = bracket' pathToSecret $ \storage -> do
    updateGtSecret storage (SetSecret_ secret)

prepareSecretToNewSlot :: (MonadMask m, MonadIO m) => Maybe FilePath -> SlotId -> m ()
prepareSecretToNewSlot pathToSecret slotId = bracket' pathToSecret $ \storage -> do
    updateGtSecret storage (PrepareSecretToNewSlot_ slotId)
