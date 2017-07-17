{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Pos.Communication.Relay.Types
       ( RelayError (..)
       , PropagationMsg (..)
       , RelayPropagationQueue
       , RelayContext (..)
       , HasPropagationFlag (..)
       , HasPropagationQueue (..)
       ) where

import           Universum

import           Control.Concurrent.STM        (TBQueue)
import           Control.Lens                  (makeLenses)
import qualified Data.Text.Buildable           as Buildable
import           Data.Time.Units               (Microsecond)
import           Formatting                    (bprint, build, (%))
import           Node                          (Message)

import           Pos.Binary.Class              (Bi)
import           Pos.Communication.Types.Relay (DataMsg, InvOrData, ReqMsg)

data RelayError = UnexpectedInv
                | UnexpectedData
  deriving (Generic, Show)

instance Exception RelayError

data PropagationMsg where
    InvReqDataPM ::
        ( Message (InvOrData key contents)
        , Bi (InvOrData key contents)
        , Buildable key
        , Eq key
        , Message (ReqMsg key)
        , Bi (ReqMsg key))
        => !key -> !contents -> PropagationMsg
    DataOnlyPM ::
        ( Message (DataMsg contents)
        , Bi (DataMsg contents)
        , Buildable contents)
        => !contents -> PropagationMsg

instance Buildable PropagationMsg where
    build (InvReqDataPM key _) =
        bprint ("<data for key "%build%">") key
    build (DataOnlyPM conts) =
        Buildable.build conts

-- | Queue of InvMsges which should be propagated.
type RelayPropagationQueue = TBQueue (Microsecond, PropagationMsg)

data RelayContext = RelayContext
    { _rlyIsPropagation    :: !Bool
    , _rlyPropagationQueue :: !RelayPropagationQueue
    }

makeLenses ''RelayContext

class HasPropagationFlag ctx where
    propagationFlag :: Lens' ctx Bool

instance HasPropagationFlag RelayContext where
    propagationFlag = rlyIsPropagation

class HasPropagationQueue ctx where
    propagationQueue :: Lens' ctx RelayPropagationQueue

instance HasPropagationQueue RelayContext where
    propagationQueue = rlyPropagationQueue
