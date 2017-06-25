module Pos.Update.Mode
       ( UpdateMode
       ) where

import           Universum

import           Control.Monad.Catch         (MonadMask)
import           EtherCompat
import           Mockable                    (MonadMockable)
import           System.Wlog                 (WithLogger)

import           Pos.Communication.PeerState (WithPeerState)
import           Pos.Communication.Relay     (MonadRelayMem)
import           Pos.DB.Class                (MonadDB, MonadGState)
import           Pos.Lrc.Context             (LrcContext)
import           Pos.Update.Context          (UpdateContext)
import           Pos.Update.Params           (UpdateParams)

type UpdateMode ctx m
    = ( WithLogger m
      , MonadMockable m
      , MonadIO m
      , WithPeerState m
      , MonadMask m
      , MonadGState m
      , MonadDB m
      , MonadRelayMem ctx m
      , MonadCtx ctx UpdateContext UpdateContext m
      , MonadCtx ctx LrcContext LrcContext m
      , MonadCtx ctx UpdateParams UpdateParams m
      )
