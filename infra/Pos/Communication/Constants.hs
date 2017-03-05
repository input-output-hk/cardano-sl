module Pos.Communication.Constants
       ( networkReceiveTimeout
       , maxReqSize
       , maxInvSize
       ) where

import           Data.Time.Units            (Microsecond)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util              (ms)
import           Universum

import           Pos.Infra.Constants.Parser (infraConstants)
import           Pos.Infra.Constants.Type   (ccMaxInvSize, ccMaxReqSize,
                                             ccNetworkReceiveTimeout)

networkReceiveTimeout :: Microsecond
networkReceiveTimeout = ms . fromIntegral . ccNetworkReceiveTimeout $ infraConstants

-- | See 'Pos.CompileConfig.ccMaxReqSize'.
maxReqSize :: Byte
maxReqSize = ccMaxReqSize infraConstants

-- | See 'Pos.CompileConfig.ccMaxInvSize'.
maxInvSize :: Byte
maxInvSize = ccMaxInvSize infraConstants
