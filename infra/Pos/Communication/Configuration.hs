module Pos.Communication.Configuration
       ( networkWaitLogInterval
       , maxReqSize
       , maxMempoolMsgSize
       , maxInvSize
       ) where

import           Data.Time.Units (Second)
import           Universum

import           Pos.Infra.Configuration (HasInfraConfiguration, ccMaxInvSize, ccMaxMempoolMsgSize,
                                          ccMaxReqSize, ccNetworkWaitLogInterval,
                                          infraConfiguration)

networkWaitLogInterval :: HasInfraConfiguration => Second
networkWaitLogInterval = fromIntegral . ccNetworkWaitLogInterval $ infraConfiguration

maxReqSize :: HasInfraConfiguration => Word32
maxReqSize = ccMaxReqSize infraConfiguration

maxMempoolMsgSize :: HasInfraConfiguration => Word32
maxMempoolMsgSize = ccMaxMempoolMsgSize infraConfiguration

maxInvSize :: HasInfraConfiguration => Word32
maxInvSize = ccMaxInvSize infraConfiguration
