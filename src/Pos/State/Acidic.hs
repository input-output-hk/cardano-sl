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
       , openMemState
       , tidyState

       , query
       , update

       , GetBlock (..)
       , GetHeadBlock (..)
       , GetLeaders (..)
       , GetLocalTxs (..)
       , GetLocalMpcData (..)
       , GetSecret (..)
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

openState :: MonadIO m => Bool -> FilePath -> m DiskState
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp def

openMemState :: MonadIO m => m DiskState
openMemState = openMemoryExtendedState def

closeState :: MonadIO m => DiskState -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => DiskState -> m ()
tidyState = tidyExtendedState

makeAcidic ''Storage
    [ 'S.getBlock
    , 'S.getLeaders
    , 'S.getLocalTxs
    , 'S.getLocalMpcData
    , 'S.getHeadBlock
    , 'S.getSecret
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
