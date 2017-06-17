module Pos.Communication.Constants
       ( networkReceiveTimeout
       , maxReqSize
       , maxMempoolMsgSize
       , maxInvSize
       ) where

import           Data.Time.Units            (Microsecond)
import           Serokell.Util              (ms)
import           Universum

import           Pos.Infra.Constants        (ccMaxInvSize, ccMaxMempoolMsgSize,
                                             ccMaxReqSize, ccNetworkReceiveTimeout,
                                             infraConstants)

networkReceiveTimeout :: Microsecond
networkReceiveTimeout = ms . fromIntegral . ccNetworkReceiveTimeout $ infraConstants

-- | See 'Pos.CompileConfig.ccMaxReqSize'.
maxReqSize :: Word32
maxReqSize = ccMaxReqSize infraConstants

-- | See 'Pos.CompileConfig.ccMaxMempoolMsgSize'.
maxMempoolMsgSize :: Word32
maxMempoolMsgSize = ccMaxMempoolMsgSize infraConstants

-- | See 'Pos.CompileConfig.ccMaxInvSize'.
maxInvSize :: Word32
maxInvSize = ccMaxInvSize infraConstants
