module Pos.Security.Class
       ( SecurityWorkersClass (..)
       ) where

import           Universum
import           Data.Tagged                (Tagged)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, NodeId)
import           Pos.Ssc.Class.Types        (Ssc)
import           Pos.WorkMode               (WorkMode)

class Ssc ssc => SecurityWorkersClass ssc where
    securityWorkers :: WorkMode ssc m => m (Set NodeId) -> Tagged ssc ([WorkerSpec m], OutSpecs)
