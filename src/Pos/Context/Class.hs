{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext
       , getNodeContext
       ) where

import qualified Control.Monad.Ether as Ether.E
import           Universum

import           Pos.Context.Context (NodeContext)
import           Pos.Util.Context    (ContextTagK (..))

-- | Class for something that has 'NodeContext' inside.
type WithNodeContext ssc = Ether.E.MonadReader 'ContextTag (NodeContext ssc)

getNodeContext :: WithNodeContext ssc m => m (NodeContext ssc)
getNodeContext = Ether.E.ask (Proxy @'ContextTag)
