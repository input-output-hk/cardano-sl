
module Pos.Communication.Specs
       ( createOutSpecs
       ) where

import           Node.Message.Class (Message (..))
import           Universum

import           Pos.Communication.Protocol (OutSpecs, convH, toOutSpecs)
import           Pos.Communication.Types.Relay (InvOrData, ReqOrRes)

-- FIXME (avieth)
-- This looks to be misplaced and misnamed.
-- Apparently it's creating the out specs for certain relay handlers.
createOutSpecs :: forall key contents .
               ( Message (InvOrData key contents)
               , Message (ReqOrRes key)
               )
               => Proxy (InvOrData key contents)
               -> OutSpecs
createOutSpecs proxy = toOutSpecs [
      convH proxy (toReqResProxy proxy)
    ]
  where
    toReqResProxy :: Proxy (InvOrData key contents) -> Proxy (ReqOrRes key)
    toReqResProxy _ = Proxy
