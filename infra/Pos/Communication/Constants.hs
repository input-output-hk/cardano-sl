module Pos.Communication.Constants
       ( maxReqSize
       , maxMempoolMsgSize
       , maxInvSize
       ) where

import           Universum

maxReqSize :: Word32
maxReqSize = 102

maxMempoolMsgSize :: Word32
maxMempoolMsgSize = 100

maxInvSize :: Word32
maxInvSize = 102
