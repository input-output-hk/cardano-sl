{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Control.TimeWarp.Rpc (BinaryP, MonadDialog)
import           Data.Tagged          (Tagged)

import           Pos.DHT              (ListenerDHT (..))
import           Pos.Ssc.Class.Types  (Ssc (..))
import           Pos.WorkMode         (WorkMode)

class Ssc ssc => SscListenersClass ssc  where
    sscListeners :: (MonadDialog BinaryP m, WorkMode ssc m)
                 => Tagged ssc [ListenerDHT m]
