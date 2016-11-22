{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Types used for communication.

module Pos.Communication.Types
       ( ResponseMode

       -- * Request types
       , module Block
       , module Tx
       , module SysStart
       , module Statistics
       , noCacheMessageNames
       ) where

import           Control.TimeWarp.Rpc               (Message (messageName), MessageName)
import           Data.Proxy                         (Proxy (..))
-- import           Universum

import           Pos.Communication.Types.Block      as Block
import           Pos.Communication.Types.Statistics as Statistics
import           Pos.Communication.Types.SysStart   as SysStart
import           Pos.Communication.Types.Tx         as Tx
import           Pos.DHT                            (MonadResponseDHT)
import           Pos.WorkMode                       (WorkMode)

type ResponseMode ssc m = (WorkMode ssc m, MonadResponseDHT m)

noCacheMessageNames :: [MessageName]
noCacheMessageNames =
    [ -- messageName (Proxy :: Proxy Block.RequestBlock)
      "RequestBlock"
    , messageName (Proxy :: Proxy SysStart.SysStartRequest)
    , messageName (Proxy :: Proxy SysStart.SysStartResponse)
    ]
