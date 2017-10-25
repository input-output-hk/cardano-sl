{-# LANGUAGE ExistentialQuantification #-}

module Lang.Command
       ( CommandProc(..)
       ) where

import           Lang.Argument (ArgumentConsumer)
import           Lang.Name     (Name)
import           Lang.Value    (Value)

data CommandProc m = forall e. CommandProc
    { cpName             :: !Name
    , cpArgumentConsumer :: !(ArgumentConsumer e)
    , cpExec             :: !(e -> m Value)
    } deriving ()
