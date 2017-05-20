{-# LANGUAGE ExistentialQuantification #-}

module Pos.Communication.Relay.Types
       ( RelayError (..)
       , SomeInvMsg (..)
       , RelayInvQueue
       , RelayContext (..)
       ) where

import           Control.Concurrent.STM        (TBQueue)
import           Node.Message                  (Message)
import           Universum

import           Pos.Binary.Class              (Bi)
import           Pos.Communication.Types.Relay (InvOrData, ReqMsg (..))

data RelayError = UnexpectedInv
                | UnexpectedData
  deriving (Generic, Show)

instance Exception RelayError

data SomeInvMsg =
    forall key contents .
        ( Message (InvOrData key contents)
        , Bi (InvOrData key contents)
        , Buildable key
        , Message (ReqMsg key)
        , Bi (ReqMsg key))
        => SomeInvMsg !(InvOrData key contents)

-- | Queue of InvMsges which should be propagated.
type RelayInvQueue = TBQueue SomeInvMsg

data RelayContext = RelayContext
    { _rlyIsPropagation    :: !Bool
    , _rlyPropagationQueue :: !RelayInvQueue
    }
