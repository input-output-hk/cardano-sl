module Pos.Communication.Limits.Instances
       ( mlInvMsg
       , mlReqMsg
       , mlResMsg
       , mlDataMsg
       , mlMempoolMsg
       ) where

import           Universum

import qualified Pos.Communication.Constants as Const
import           Pos.Communication.Limits.Types (Limit (..))
import           Pos.Communication.Types.Relay (InvMsg, DataMsg (..), MempoolMsg (..),
                                                ReqMsg, ResMsg)

----------------------------------------------------------------------------
-- Instances of MessageLimited for the relay types.
-- TODO make these all obsolete. Their Bi instances should fail if too much
-- input is given...
----------------------------------------------------------------------------

mlInvMsg :: Limit (InvMsg key)
mlInvMsg = Limit Const.maxInvSize

mlReqMsg :: Limit (ReqMsg key)
-- Add 1 because ReqMsg contains a 'Maybe key'
mlReqMsg = Limit (Const.maxReqSize + 1)

mlResMsg :: Limit (ResMsg key)
-- It's a ResMsg key, with an extra bool, and overhead for the tuple.
mlResMsg = Limit (Const.maxReqSize + 2)

-- TBD is this right? Surely data msg should add some extra bytes.
mlDataMsg :: Limit a -> Limit (DataMsg a)
mlDataMsg = fmap DataMsg

mlMempoolMsg :: Limit (MempoolMsg tag)
mlMempoolMsg = Limit Const.maxMempoolMsgSize
