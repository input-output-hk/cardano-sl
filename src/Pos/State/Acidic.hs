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

       , NewStatRecord (..)
       , GetStatRecords (..)
       ) where

import           Data.Acid            (EventResult, EventState, QueryEvent, UpdateEvent,
                                       makeAcidicWithHacks)
import           Data.Default         (Default, def)
import           Data.SafeCopy        (SafeCopy)
import           Serokell.AcidState   (ExtendedState, closeExtendedState,
                                       openLocalExtendedState, openMemoryExtendedState,
                                       queryExtended, tidyExtendedState, updateExtended)
import           Universum

import qualified Pos.State.Storage    as S
import           Pos.Ssc.Class.Types (SscTypes (SscStorage))
import           Pos.Ssc.Class.Storage (SscStorageClass (..))

----------------------------------------------------------------------------
-- Acid-state things
----------------------------------------------------------------------------

type Storage ssc = S.Storage ssc
type DiskState ssc = ExtendedState (Storage ssc)

query
    :: (SscStorageClass ssc, EventState event ~ Storage ssc,
        QueryEvent event, MonadIO m)
    => DiskState ssc -> event -> m (EventResult event)
query = queryExtended

update
    :: (SscStorageClass ssc, EventState event ~ Storage ssc,
        UpdateEvent event, MonadIO m)
    => DiskState ssc -> event -> m (EventResult event)
update = updateExtended


-- | Same as `openState`, but with explicitly specified initial state.
openStateCustom :: (SscStorageClass ssc, SafeCopy ssc, MonadIO m)
                => Storage ssc -> Bool -> FilePath -> m (DiskState ssc)
openStateCustom customStorage deleteIfExists fp =
    openLocalExtendedState deleteIfExists fp customStorage

openState :: (SscStorageClass ssc,
              Default (SscStorage ssc),
              SafeCopy ssc,
              MonadIO m)
          => Bool -> FilePath -> m (DiskState ssc)
openState = openStateCustom def

openMemState :: (SscStorageClass ssc,
                 Default (SscStorage ssc),
                 SafeCopy ssc,
                 MonadIO m)
             => m (DiskState ssc)
openMemState = openMemStateCustom def

openMemStateCustom :: (SscStorageClass ssc,
                       SafeCopy ssc,
                       MonadIO m)
                   => Storage ssc -> m (DiskState ssc)
openMemStateCustom = openMemoryExtendedState

closeState :: (SscStorageClass ssc, MonadIO m) => DiskState ssc -> m ()
closeState = closeExtendedState

tidyState :: (SscStorageClass ssc, MonadIO m) => DiskState ssc -> m ()
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
    , 'S.newStatRecord
    , 'S.getStatRecords
    ]
