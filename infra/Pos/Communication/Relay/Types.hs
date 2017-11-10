{-# LANGUAGE GADTs #-}

module Pos.Communication.Relay.Types
       ( RelayError (..)
       , PropagationMsg (..)
       ) where

import           Prelude (Show (..))
import           Universum hiding (Show)

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))
import           Node (Message)

import           Pos.Binary.Class (Bi)
import           Pos.Communication.Types.Protocol (Msg)
import           Pos.Communication.Types.Relay (DataMsg, InvOrData, ReqOrRes)

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
        , Message (ReqOrRes key)
        , Bi (ReqOrRes key))
        => !Msg
        -> !key
        -> !contents
        -> PropagationMsg
    DataOnlyPM ::
        ( Message (DataMsg contents)
        , Bi (DataMsg contents)
        , Buildable contents)
        => !Msg
        -> !contents
        -> PropagationMsg

instance Buildable PropagationMsg where
    build (InvReqDataPM _ key _) =
        bprint ("<data for key "%build%">") key
    build (DataOnlyPM _ conts) =
        Buildable.build conts
