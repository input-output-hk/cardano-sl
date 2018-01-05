module Pos.Communication.Configuration
       ( networkWaitLogInterval
       , maxReqSize
       , maxMempoolMsgSize
       , maxInvSize
       ) where

import           Data.Time.Units (Second)
import           Universum

import           Pos.Infra.Configuration (HasInfraConfiguration, 
                                          ccNetworkWaitLogInterval,
                                          infraConfiguration)

networkWaitLogInterval :: HasInfraConfiguration => Second
networkWaitLogInterval = fromIntegral . ccNetworkWaitLogInterval $ infraConfiguration

maxReqSize :: Word32
maxReqSize = 102

maxMempoolMsgSize :: Word32
maxMempoolMsgSize = 100

maxInvSize :: Word32
maxInvSize = 102
