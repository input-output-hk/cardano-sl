module Pos.Communication.Configuration
       ( maxReqSize
       , maxMempoolMsgSize
       , maxInvSize
       ) where

import           Universum

import           Pos.Infra.Configuration (HasInfraConfiguration, ccMaxInvSize, ccMaxMempoolMsgSize,
                                          ccMaxReqSize, infraConfiguration)

maxReqSize :: HasInfraConfiguration => Word32
maxReqSize = ccMaxReqSize infraConfiguration

maxMempoolMsgSize :: HasInfraConfiguration => Word32
maxMempoolMsgSize = ccMaxMempoolMsgSize infraConfiguration

maxInvSize :: HasInfraConfiguration => Word32
maxInvSize = ccMaxInvSize infraConfiguration
