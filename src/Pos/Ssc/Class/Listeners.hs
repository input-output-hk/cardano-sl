{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes #-}

-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Data.Tagged                   (Tagged)

import           Pos.Communication.Types.State (MutPeerState)
import           Pos.DHT.Model                 (ListenerDHT (..), MonadDHTDialog)
import           Pos.Ssc.Class.Types           (Ssc (..))
import           Pos.WorkMode                  (NewWorkMode)
import           Pos.NewDHT.Model.Class        (MonadDHT (..))
import           Mockable.Monad                (MonadMockable (..))
import           Node                          (ListenerAction (..))
import           Pos.Communication.BiP         (BiP (..))
import           Pos.Binary.Class              (Bi)

-- | Class for defining listeners in DHT @SSC@ implementation.
class Ssc ssc => SscListenersClass ssc where
    sscListeners
        :: forall m .
           ( MonadDHT m
           , MonadMockable m
           , NewWorkMode ssc m
           )
        => Tagged ssc [ListenerAction BiP m]
