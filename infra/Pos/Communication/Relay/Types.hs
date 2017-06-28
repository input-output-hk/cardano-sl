{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pos.Communication.Relay.Types
       ( RelayError (..)
       , PropagationMsg (..)
       , RelayContext (..)
       , hoistRelayContext
       ) where

import qualified Data.Text.Buildable           as Buildable
import           Data.Time.Units               (Microsecond)
import           Formatting                    (bprint, build, (%))
import           Node                          (Message)
import           Universum

import           Pos.Binary.Class              (Bi)
import           Pos.Communication.Types.Protocol (NodeId)
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

data RelayContext m = RelayContext
    { _rlyIsPropagation :: !Bool
    , _rlyEnqueue :: !(PropagationMsg -> Maybe NodeId -> m ())
    , _rlyDequeue :: !(m (PropagationMsg, Maybe NodeId, NodeId, Microsecond))
    }

hoistRelayContext
    :: (forall a . m a -> n a)
    -> RelayContext m
    -> RelayContext n
hoistRelayContext nat rc = rc
    { _rlyEnqueue = \msg provenance -> nat (_rlyEnqueue rc msg provenance)
    , _rlyDequeue = nat (_rlyDequeue rc)
    }
