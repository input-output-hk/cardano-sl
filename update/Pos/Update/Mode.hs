module Pos.Update.Mode
       ( UpdateMode
       ) where

import           Universum

import           Control.Monad.Catch         (MonadMask)
import qualified Ether
import           Mockable                    (MonadMockable)
import           System.Wlog                 (WithLogger)

import           Pos.Communication.PeerState (WithPeerState)
import           Pos.Communication.Relay     (MonadRelayMem)
import           Pos.DB.Class                (MonadDB, MonadGState, MonadRealDB)
import           Pos.Lrc.Context             (LrcContext)
import           Pos.Update.Context          (UpdateContext)
import           Pos.Update.Params           (UpdateParams)

type UpdateMode m
    = ( WithLogger m
      , MonadMockable m
      , MonadIO m
      , WithPeerState m
      , MonadMask m
      , MonadGState m
      , MonadRealDB m
      , MonadDB m
      , MonadRelayMem m
      , Ether.MonadReader' UpdateContext m
      , Ether.MonadReader' LrcContext m
      , Ether.MonadReader' UpdateParams m
      )
