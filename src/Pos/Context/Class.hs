{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext
       , getNodeContext
       , NodeContextTagK(..)
       ) where

import qualified Control.Monad.Ether as Ether.E
import           Universum

import           Pos.Context.Context (NodeContext)

-- TODO: Remove DataKinds shenanigans.
data NodeContextTagK = NodeContextTag

-- | Class for something that has 'NodeContext' inside.
type WithNodeContext ssc = Ether.E.MonadReader 'NodeContextTag (NodeContext ssc)

getNodeContext :: WithNodeContext ssc m => m (NodeContext ssc)
getNodeContext = Ether.E.ask (Proxy @'NodeContextTag)
