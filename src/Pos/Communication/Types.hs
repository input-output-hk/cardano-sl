{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Types used for communication.

module Pos.Communication.Types
       ( ResponseMode

         -- * Request types
       , module Pos.Communication.Types.Block
       , module Pos.Communication.Types.SysStart
       , module Pos.Txp.Types.Communication

       , noCacheMessageNames
       ) where

import           Control.TimeWarp.Rpc             (Message (messageName), MessageName)
import           Data.Proxy                       (Proxy (..))

import           Pos.Communication.Types.Block
import           Pos.Communication.Types.SysStart
import           Pos.DHT                          (MonadResponseDHT)
import           Pos.Txp.Types.Communication
import           Pos.WorkMode                     (SocketState, WorkMode)

-- | Constraint alias for 'WorkMode' with 'MonadResponseDHT'.
type ResponseMode ssc m = (WorkMode ssc m, MonadResponseDHT SocketState m)

-- | 'MessageName'`s that shouldn't be cached.
noCacheMessageNames :: [MessageName]
noCacheMessageNames =
    [ -- messageName (Proxy :: Proxy Block.RequestBlock)
      "RequestBlock"
    , messageName (Proxy :: Proxy SysStartRequest)
    , messageName (Proxy :: Proxy SysStartResponse)
    ]
