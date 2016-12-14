{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Socket state of server.

module Pos.Communication.Types.State
       ( SocketState
       , MutSocketState
       , newMutSocketState
       ) where

import           Control.Concurrent.STM         (TVar, newTVarIO)
import           Control.Lens                   (makeClassy)
import           Data.Default                   (Default (def))
import           Universum

#ifdef WITH_ROCKS
import           Pos.Modern.Blockp.Server.State (BlockSocketState)
#else
type BlockSocketState ssc = ()
#endif

-- | SocketState type aggregates socket states needed for different
-- parts of system.
data SocketState ssc = SocketState
    { __blockSocketState :: !(BlockSocketState ssc)
    }

-- | Classy lenses for SocketState.
makeClassy ''SocketState

instance Default (SocketState ssc) where
    def =
        SocketState
        { __blockSocketState = def
        }

-- | Mutable SocketState.
type MutSocketState ssc = TVar (SocketState ssc)

newMutSocketState :: IO (MutSocketState ssc)
newMutSocketState = newTVarIO def
