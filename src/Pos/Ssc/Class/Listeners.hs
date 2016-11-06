{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Control.TimeWarp.Rpc (BinaryP, MonadDialog)
import           Data.Tagged          (Tagged)

import           Pos.DHT              (ListenerDHT (..))
import           Pos.Ssc.Class.Types  (SscTypes (..))
import           Pos.WorkMode         (WorkMode)

class SscTypes ssc => SscListenersClass ssc  where
    sscListeners :: (MonadDialog BinaryP m, WorkMode m)
                 => Tagged ssc [ListenerDHT m]
