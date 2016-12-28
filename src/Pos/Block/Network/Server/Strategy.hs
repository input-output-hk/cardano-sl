{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | ForkStrategy of block server.

module Pos.Block.Network.Server.Strategy
       ( blkForkStrategy
       ) where

import           Control.TimeWarp.Rpc    (MessageName, messageName)
import           Data.Proxy              (Proxy (Proxy))
import           Universum

import           Pos.Block.Network.Types (MsgBlock)

-- | ForkStrategy of block server. Defines how to spawn handler for message.
blkForkStrategy
    :: forall ssc m.
       Typeable ssc
    => MessageName -> Maybe (m () -> m ())
blkForkStrategy msgName
    | msgName == messageName (Proxy @(MsgBlock ssc)) = Just identity
    | otherwise = Nothing
