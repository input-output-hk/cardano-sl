{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Types used for communication.

module Pos.Communication.Types
       ( ResponseMode

         -- * Message types
       , module Pos.Communication.Types.Block
       , module Pos.Communication.Types.SysStart
       , module Pos.Txp.Types.Communication

         -- * Socket state
       , module Pos.Communication.Types.State

       , noCacheMessageNames
       ) where

import           Control.TimeWarp.Rpc             (Message (messageName), MessageName)
import           Data.Proxy                       (Proxy (..))

import           Pos.Communication.Types.Block
import           Pos.Communication.Types.State
import           Pos.Communication.Types.SysStart
import           Pos.DHT.Model                    (MonadResponseDHT)
import           Pos.Txp.Types.Communication
import           Pos.WorkMode                     (WorkMode)

-- | Constraint alias for 'WorkMode' with 'MonadResponseDHT'.
type ResponseMode ssc m = (WorkMode ssc m, MonadResponseDHT (MutSocketState ssc) m)

-- | 'MessageName'`s that shouldn't be cached.
noCacheMessageNames :: [MessageName]
noCacheMessageNames =
    [ -- messageName (Proxy :: Proxy Block.RequestBlock)
      "RequestBlock"
    , messageName (Proxy :: Proxy SysStartRequest)
    , messageName (Proxy :: Proxy SysStartResponse)
    ]
