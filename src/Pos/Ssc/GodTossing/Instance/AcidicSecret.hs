{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

-- | @acid-state@ implementations of secret storage state for @GodTossing@.

module Pos.Ssc.GodTossing.Instance.AcidicSecret
       (
         getSecret
       , setSecret
       , prepareSecretToNewSlot
       ) where


import           Data.Acid                                 (makeAcidic)
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

type SecQuery'  a = forall m . MonadReader GtSecretStorage m => m a
type SecUpdate' a = forall m . MonadState GtSecretStorage m => m a

getSecret_ :: SecQuery' (Maybe GtSecret)
getSecret_ = ssGetSecret @GtSecretStorage

setSecret_ :: GtSecret -> SecUpdate' ()
setSecret_ = ssSetSecret @GtSecretStorage

prepareSecretToNewSlot_ :: SlotId -> SecUpdate' ()
prepareSecretToNewSlot_ = ssPrepareToNewSlot @GtSecretStorage


makeAcidic ''GtSecretStorage
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

-- | Get 'GtSecret' from from storage by given 'FilePath'.
getSecret :: (MonadMask m, MonadIO m) => Maybe FilePath -> m (Maybe GtSecret)
getSecret pathToSecret = bracket' pathToSecret $ flip queryExtended GetSecret_

-- | Update 'GtSecret' with given value in storage by given 'FilePath'.
setSecret :: (MonadMask m, MonadIO m) => Maybe FilePath -> GtSecret -> m ()
setSecret pathToSecret secret = bracket' pathToSecret $
    flip updateExtended (SetSecret_ secret)

-- | Prepare 'GtSecret' for new slot with given 'SlotId'.
prepareSecretToNewSlot :: (MonadMask m, MonadIO m) => Maybe FilePath -> SlotId -> m ()
prepareSecretToNewSlot pathToSecret slotId = bracket' pathToSecret $
    flip updateExtended (PrepareSecretToNewSlot_ slotId)
