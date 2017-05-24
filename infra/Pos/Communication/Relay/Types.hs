{-# LANGUAGE GADTs #-}

module Pos.Communication.Relay.Types
       ( RelayError (..)
       , PropagationMsg (..)
       , RelayPropagationQueue
       , RelayContext (..)
       ) where

import           Control.Concurrent.STM        (TBQueue)
import qualified Data.Text.Buildable           as Buildable
import           Formatting                    (bprint, build, (%))
import           Node.Message                  (Message)
import           Universum

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
type RelayPropagationQueue = TBQueue PropagationMsg

data RelayContext = RelayContext
    { _rlyIsPropagation    :: !Bool
    , _rlyPropagationQueue :: !RelayPropagationQueue
    }
