module Pos.Communication.Constants
       ( networkReceiveTimeout
       , maxReqSize
       , maxMempoolMsgSize
       , maxInvSize
       ) where

import           Data.Time.Units            (Microsecond)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util              (ms)
import           Universum

import           Pos.Infra.Constants        (ccMaxInvSize, ccMaxMempoolMsgSize,
                                             ccMaxReqSize, ccNetworkReceiveTimeout,
                                             infraConstants)

networkReceiveTimeout :: Microsecond
networkReceiveTimeout = ms . fromIntegral . ccNetworkReceiveTimeout $ infraConstants

-- | See 'Pos.CompileConfig.ccMaxReqSize'.
maxReqSize :: Byte
maxReqSize = ccMaxReqSize infraConstants

-- | See 'Pos.CompileConfig.ccMaxMempoolMsgSize'.
maxMempoolMsgSize :: Byte
maxMempoolMsgSize = ccMaxMempoolMsgSize infraConstants

-- | See 'Pos.CompileConfig.ccMaxInvSize'.
maxInvSize :: Byte
maxInvSize = ccMaxInvSize infraConstants
