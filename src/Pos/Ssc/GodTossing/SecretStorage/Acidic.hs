{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Pos.Ssc.GodTossing.SecretStorage.Acidic
       (
         openGtSecretStorage
       , openMemGtSecretStorage
       , closeSecretStorage
       , SecretStorage
       , GetS (..)
       , SetS (..)
       , Prepare (..)
       ) where

import           Control.Monad.State                    (get, put)
import           Data.Acid                              (Query, Update, makeAcidic)
import           Data.Default                           (def)
import           Serokell.AcidState                     (ExtendedState,
                                                         closeExtendedState,
                                                         openLocalExtendedState,
                                                         openMemoryExtendedState)
import           Universum

import           Pos.Ssc.GodTossing.SecretStorage.Types (GtSecret, GtSecretStorage (..))
import           Pos.Types                              (SlotId (..))


getS :: Query GtSecretStorage (Maybe GtSecret)
getS = asks _dsCurrentSecret

setS :: GtSecret -> Update GtSecretStorage ()
setS (ourPk, comm, op) = do
    st <- get
    case _dsCurrentSecret st of
        Just _  -> pass
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
type SecretStorage  = ExtendedState GtSecretStorage

openGtSecretStorage :: MonadIO m
                  => Bool
                  -> FilePath
                  -> m SecretStorage
openGtSecretStorage deleteIfExists fp =
    openLocalExtendedState deleteIfExists fp def


openMemGtSecretStorage :: MonadIO m
             => m SecretStorage
openMemGtSecretStorage = openMemoryExtendedState def

closeSecretStorage :: MonadIO m => SecretStorage -> m ()
closeSecretStorage = closeExtendedState

makeAcidic ''GtSecretStorage
    [
      'getS
    , 'setS
    , 'prepare
    ]

