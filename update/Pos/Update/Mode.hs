{-# LANGUAGE ConstraintKinds #-}

module Pos.Update.Mode
       ( UpdateMode
       ) where

import           Universum

import           Control.Monad.Catch         (MonadMask)
import           Mockable                    (MonadMockable)
import           System.Wlog                 (WithLogger)

import           Pos.Communication.PeerState (WithPeerState)
import           Pos.Communication.Relay     (MonadRelayMem)
import           Pos.DB.Class                (MonadDB)
import           Pos.DB.Limits               (MonadDBLimits)
import           Pos.Lrc.Context             (LrcContext)
import           Pos.Update.Context          (UpdateContext)
import           Pos.Update.Params           (UpdateParams)
import           Pos.Util.Context            (HasContext)

type UpdateMode m
    = ( WithLogger m
      , MonadMockable m
      , MonadIO m
      , WithPeerState m
      , MonadMask m
      , MonadDB m
      , MonadDBLimits m
      , MonadRelayMem m
      , HasContext UpdateContext m
      , HasContext LrcContext m
      , HasContext UpdateParams m
      )
