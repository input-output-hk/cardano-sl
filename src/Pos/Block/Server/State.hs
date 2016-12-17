{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Socket state of block processing server.

module Pos.Block.Server.State
       ( BlockSocketState
       , HasBlockSocketState (blockSocketState)
       , bssRequestedBlocks
       ) where

import           Control.Lens (makeClassy)
import           Data.Default (Default (def))
import           Universum

import           Pos.Types    (HeaderHash)

data BlockSocketState ssc = BlockSocketState
    { _bssRequestedBlocks :: [HeaderHash ssc]
    }

-- | Classy lenses generated for BlockSocketState.
makeClassy ''BlockSocketState

instance Default (BlockSocketState ssc) where
    def =
        BlockSocketState
        { _bssRequestedBlocks = mempty
        }
