module Pos.Security.Class
       ( SecurityWorkersClass (..)
       ) where

import           Data.Tagged                      (Tagged)
import           Node                             (Worker)
-- import           Universum

import           Pos.Communication.BiP            (BiP)
import           Pos.Communication.Types.Protocol (PeerId)
import           Pos.Ssc.Class.Types              (Ssc)
import           Pos.WorkMode                     (WorkMode)

class Ssc ssc => SecurityWorkersClass ssc where
    securityWorkers :: WorkMode ssc m => Tagged ssc [Worker BiP PeerId m]
