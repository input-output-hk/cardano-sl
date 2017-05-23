{-# LANGUAGE ExistentialQuantification #-}

module Pos.Communication.Relay.Types
       ( RelayProxy (..)
       , RelayError (..)
       , SomeInvMsg (..)
       , RelayInvQueue
       , RelayContext (..)
       ) where

import           Control.Concurrent.STM        (TBQueue)
import           Node.Message                  (Message)
import           Universum

import           Pos.Binary.Class              (Bi)
import           Pos.Communication.Types.Relay (InvOrData, ReqMsg (..))

data RelayProxy key tag contents = RelayProxy

data RelayError = UnexpectedInv
                | UnexpectedData
  deriving (Generic, Show)

instance Exception RelayError

data SomeInvMsg =
    forall tag key contents .
        ( Message (InvOrData tag key contents)
        , Bi (InvOrData tag key contents)
        , Buildable tag
        , Buildable key
        , Eq key
        , Message (ReqMsg key tag)
        , Bi (ReqMsg key tag))
        => SomeInvMsg !tag !key !contents

-- | Queue of InvMsges which should be propagated.
type RelayInvQueue = TBQueue SomeInvMsg

data RelayContext = RelayContext
    { _rlyIsPropagation    :: !Bool
    , _rlyPropagationQueue :: !RelayInvQueue
    }
