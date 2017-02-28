module Pos.Communication.Constants
       ( networkReceiveTimeout
       ) where

import           Data.Time.Units            (Microsecond)
import           Serokell.Util.Time         (ms)
import           Universum

import           Pos.Infra.Constants.Parser (infraConstants)
import           Pos.Infra.Constants.Type   (ccNetworkReceiveTimeout)

networkReceiveTimeout :: Microsecond
networkReceiveTimeout = ms . fromIntegral . ccNetworkReceiveTimeout $ infraConstants
