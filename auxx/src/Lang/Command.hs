{-# LANGUAGE ExistentialQuantification #-}

module Lang.Command
       ( CommandProc(..)
       , UnavailableCommand(..)
       ) where

import           Universum

import           Lang.Argument (ArgumentConsumer)
import           Lang.Name (Name)
import           Lang.Value (Value)
import           Lang.Syntax (Arg)

data CommandProc m = forall e. CommandProc
    { cpName             :: !Name
    , cpArgumentPrepare  :: !([Arg Value] -> [Arg Value])
    , cpArgumentConsumer :: !(ArgumentConsumer e)
    , cpExec             :: !(e -> m Value)
    , cpHelp             :: !Text
    } deriving ()

data UnavailableCommand = UnavailableCommand
    { ucName :: !Name
    , ucReason :: !Text
    }
