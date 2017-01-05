{-# LANGUAGE TemplateHaskell #-}

-- | Full in-memory state of Update System. Consists of global state
-- and local state.

module Pos.Update.State.Full
       ( MemState (..)
       ) where

import           Control.Lens            (makeLenses)
import           Data.Default            (Default (def))
import           Universum

import           Pos.Update.State.Global (GlobalState, HasGlobalState (globalState))
import           Pos.Update.State.Local  (HasLocalState (localState), LocalState)

data MemState = MemState
    { _msGlobal :: !GlobalState
    , _msLocal  :: !LocalState
    }

makeLenses ''MemState

instance Default MemState where
    def =
        MemState
        { _msGlobal = def
        , _msLocal = def
        }

instance HasGlobalState MemState where
    globalState = msGlobal

instance HasLocalState MemState where
    localState = msLocal
