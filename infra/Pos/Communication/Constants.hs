module Pos.Communication.Constants
       ( networkWaitLogInterval
       , maxReqSize
       , maxMempoolMsgSize
       , maxInvSize
       ) where

import           Data.Time.Units     (Second)
import           Universum

import           Pos.Infra.Constants (ccMaxInvSize, ccMaxMempoolMsgSize, ccMaxReqSize,
                                      ccNetworkWaitLogInterval, infraConstants)

networkWaitLogInterval :: Second
networkWaitLogInterval = fromIntegral . ccNetworkWaitLogInterval $ infraConstants

-- | See 'Pos.CompileConfig.ccMaxReqSize'.
maxReqSize :: Word32
maxReqSize = ccMaxReqSize infraConstants

-- | See 'Pos.CompileConfig.ccMaxMempoolMsgSize'.
maxMempoolMsgSize :: Word32
maxMempoolMsgSize = ccMaxMempoolMsgSize infraConstants

-- | See 'Pos.CompileConfig.ccMaxInvSize'.
maxInvSize :: Word32
maxInvSize = ccMaxInvSize infraConstants
