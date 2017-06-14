{-# LANGUAGE FlexibleContexts #-}

module Network.Broadcast.Relay.Util
       ( expectInv
       , expectData
       ) where

import           Mockable                      (Mockable, Throw, throw)

import           Network.Broadcast.Relay.Types (DataMsg, InvMsg, InvOrData,
                                                RelayError(..))

expectInv
    :: Mockable Throw m
    => (InvMsg key -> m a) -> InvOrData key contents -> m a
expectInv call = either call (\_ -> throw UnexpectedData)

expectData
    :: Mockable Throw m
    => (DataMsg contents -> m a) -> InvOrData key contents -> m a
expectData call = either (\_ -> throw UnexpectedInv) call
