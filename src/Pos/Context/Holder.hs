{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of WithNodeContext.

module Pos.Context.Holder
       ( ContextHolder
       , runContextHolder
       ) where

import qualified Ether
import           Universum

import           Pos.Context.Context (NodeContext (..))

-- | Wrapper for monadic action which brings 'NodeContext'.
type ContextHolder ssc = Ether.ReadersT (NodeContext ssc)

-- | Run 'ContextHolder' action.
runContextHolder :: NodeContext ssc -> ContextHolder ssc m a -> m a
runContextHolder = flip Ether.runReadersT

