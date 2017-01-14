module Pos.Security.Class
       ( SecurityWorkersClass (..)
       ) where

import           Data.Tagged           (Tagged)
import           Node                  (SendActions)
-- import           Universum

import           Pos.Communication.BiP (BiP)
import           Pos.Ssc.Class.Types   (Ssc)
import           Pos.WorkMode          (WorkMode)

class Ssc ssc => SecurityWorkersClass ssc where
    securityWorkers :: WorkMode ssc m => Tagged ssc [SendActions BiP m -> m ()]
