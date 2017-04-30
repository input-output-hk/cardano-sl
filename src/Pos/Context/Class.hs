{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext
       , getNodeContext
       ) where

import qualified Ether

import           Pos.Context.Context (NodeContext, NodeContextTag)

-- | Class for something that has 'NodeContext' inside.
type WithNodeContext ssc = Ether.MonadReader NodeContextTag (NodeContext ssc)

getNodeContext :: WithNodeContext ssc m => m (NodeContext ssc)
getNodeContext = Ether.ask @NodeContextTag
