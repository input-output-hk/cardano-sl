module Pos.Communication.Relay.Util
       ( expectInv
       , expectData
       ) where

import           Universum

import           Mockable                      (Mockable, Throw, throw)

import           Pos.Communication.Relay.Types (RelayError (UnexpectedData, UnexpectedInv))
import           Pos.Communication.Types.Relay (DataMsg, InvMsg, InvOrData)

expectInv
    :: Mockable Throw m
    => (InvMsg key -> m a) -> InvOrData key contents -> m a
expectInv call = either call (\_ -> throw UnexpectedData)

expectData
    :: Mockable Throw m
    => (DataMsg contents -> m a) -> InvOrData key contents -> m a
expectData call = either (\_ -> throw UnexpectedInv) call
