{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Communication.Specs
       ( createOutSpecs
       ) where

import           Node.Message                  (Message (..))
import           Universum

import           Pos.Communication.Protocol    (OutSpecs, convH, toOutSpecs)
import           Pos.Communication.Types.Relay (InvOrData, ReqMsg)

createOutSpecs :: forall key contents .
               ( Message (InvOrData key contents)
               , Message (ReqMsg key))
               => Proxy (InvOrData key contents)
               -> OutSpecs
createOutSpecs proxy = toOutSpecs [convH proxy (toReqProxy proxy)]
  where
    toReqProxy :: Proxy (InvOrData key contents) -> Proxy (ReqMsg key)
    toReqProxy _ = Proxy
