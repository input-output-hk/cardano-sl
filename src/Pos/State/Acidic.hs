{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
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
       , GetHeadBlock (..)
       , GetLeaders (..)
       , GetLocalTxs (..)
       , GetLocalMpcData (..)
       , GetGlobalMpcData (..)
       , GetSecret (..)
       , GetOurCommitment (..)
       , GetOurOpening (..)
       , GetOurShares (..)
       , GetThreshold (..)
       , GetParticipants (..)
       , MayBlockBeUseful (..)

       , CreateNewBlock (..)
       , ProcessBlock (..)
       , ProcessNewSlot (..)
       , ProcessCommitment (..)
       , ProcessOpening (..)
       , ProcessShares (..)
       , ProcessTx (..)
       , ProcessVssCertificate (..)
       , SetSecret (..)

       , AddStatRecord (..)
       , GetStatRecords (..)
       ) where

import           Data.Acid          (EventResult, EventState, QueryEvent, UpdateEvent,
                                     makeAcidic)
import           Data.Default       (def)
import           Serokell.AcidState (ExtendedState, closeExtendedState,
                                     openLocalExtendedState, openMemoryExtendedState,
                                     queryExtended, tidyExtendedState, updateExtended)
import           Universum

import           Pos.State.Storage  (Storage)
import qualified Pos.State.Storage  as S

----------------------------------------------------------------------------
-- Acid-state things
----------------------------------------------------------------------------

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

makeAcidic ''Storage
    [ 'S.getBlock
    , 'S.getLeaders
    , 'S.getLocalTxs
    , 'S.getLocalMpcData
    , 'S.getGlobalMpcData
    , 'S.getHeadBlock
    , 'S.getSecret
    , 'S.getOurCommitment
    , 'S.getOurOpening
    , 'S.getOurShares
    , 'S.getThreshold
    , 'S.getParticipants
    , 'S.mayBlockBeUseful
    , 'S.createNewBlock
    , 'S.processBlock
    , 'S.processNewSlot
    , 'S.processCommitment
    , 'S.processOpening
    , 'S.processShares
    , 'S.processTx
    , 'S.processVssCertificate
    , 'S.setSecret
    , 'S.addStatRecord
    , 'S.getStatRecords
    ]
