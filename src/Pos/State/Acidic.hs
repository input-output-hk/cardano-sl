{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Acid-state wrapped operations.

module Pos.State.Acidic
       ( DiskState
       , closeState
       , openState
       , openStateCustom
       , openMemState
       , openMemStateCustom
       , tidyState

       , query
       , update

       , GetBlock (..)
       , GetGlobalSscPayload (..)
       , GetHeadBlock (..)
       , GetLeaders (..)
       , GetLocalTxs (..)
       , GetLocalSscPayload (..)
       , GetToken (..)
       , GetOurShares (..)
       , GetThreshold (..)
       , GetParticipants (..)
       , MayBlockBeUseful (..)

       , CreateNewBlock (..)
       , ProcessBlock (..)
       , ProcessNewSlot (..)
       , ProcessSscMessage (..)
       , ProcessTx (..)
       , SetToken (..)

       , AddStatRecord (..)
       , GetStatRecords (..)
       ) where

import           Data.Acid            (EventResult, EventState, QueryEvent, UpdateEvent,
                                       makeAcidicWithHacks)
import           Data.Default         (def)
import           Serokell.AcidState   (ExtendedState, closeExtendedState,
                                       openLocalExtendedState, openMemoryExtendedState,
                                       queryExtended, tidyExtendedState, updateExtended)
import           Universum

import           Pos.Ssc.DynamicState (SscDynamicState)
import qualified Pos.State.Storage    as S

----------------------------------------------------------------------------
-- Acid-state things
----------------------------------------------------------------------------

type Storage = S.Storage SscDynamicState
type DiskState = ExtendedState Storage

query
    :: (EventState event ~ Storage, QueryEvent event, MonadIO m)
    => DiskState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ Storage, UpdateEvent event, MonadIO m)
    => DiskState -> event -> m (EventResult event)
update = updateExtended

-- | Same as `openState`, but with explicitly specified initial state.
openStateCustom :: MonadIO m => Storage -> Bool -> FilePath -> m DiskState
openStateCustom customStorage deleteIfExists fp =
    openLocalExtendedState deleteIfExists fp customStorage

openState :: MonadIO m => Bool -> FilePath -> m DiskState
openState = openStateCustom def

openMemState :: MonadIO m => m DiskState
openMemState = openMemStateCustom def

openMemStateCustom :: MonadIO m => Storage -> m DiskState
openMemStateCustom = openMemoryExtendedState

closeState :: MonadIO m => DiskState -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => DiskState -> m ()
tidyState = tidyExtendedState

makeAcidicWithHacks ''S.Storage ["ssc"]
    [ 'S.getBlock
    , 'S.getGlobalSscPayload
    , 'S.getLeaders
    , 'S.getLocalSscPayload
    , 'S.getLocalTxs
    , 'S.getHeadBlock
    , 'S.getToken
    , 'S.getOurShares
    , 'S.getThreshold
    , 'S.getParticipants
    , 'S.mayBlockBeUseful
    , 'S.createNewBlock
    , 'S.processBlock
    , 'S.processNewSlot
    , 'S.processSscMessage
    , 'S.processTx
    , 'S.setToken
    , 'S.addStatRecord
    , 'S.getStatRecords
    ]
