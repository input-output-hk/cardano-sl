{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Types used for communication.

module Pos.Communication.Types
       ( ResponseMode

         -- * Messages and socket state
       , module Export

       , noCacheMessageNames

       , SendProxySecretKey (..)
       ) where

import           Control.TimeWarp.Rpc             (Message (..), MessageName,
                                                   messageName')
import           Data.Proxy                       (Proxy (..))
import           Universum

import           Pos.Communication.Types.State    (MutSocketState)
import qualified Pos.Communication.Types.SysStart as SysStart
import           Pos.Crypto                       (ProxySecretKey)
import           Pos.DHT.Model                    (MonadResponseDHT)
import           Pos.Types                        (EpochIndex)
import           Pos.WorkMode                     (WorkMode)

import           Pos.Communication.Types.Block    as Export
import           Pos.Communication.Types.State    as Export
import           Pos.Communication.Types.SysStart as Export
import           Pos.Txp.Types.Communication      as Export

-- | Constraint alias for 'WorkMode' with 'MonadResponseDHT'.
type ResponseMode ssc m = (WorkMode ssc m, MonadResponseDHT (MutSocketState ssc) m)

-- | 'MessageName'`s that shouldn't be cached.
noCacheMessageNames :: [MessageName]
noCacheMessageNames =
    [ -- messageName (Proxy :: Proxy Block.RequestBlock)
      "RequestBlock"
    , messageName (Proxy :: Proxy SysStart.SysStartRequest)
    , messageName (Proxy :: Proxy SysStart.SysStartResponse)
    ]

----------------------------------------------------------------------------
-- Certificate
----------------------------------------------------------------------------

-- | Message: some node has sent a Block.
data SendProxySecretKey =
    SendProxySecretKey !(ProxySecretKey (EpochIndex, EpochIndex))
    deriving (Generic)

instance Message SendProxySecretKey where
    messageName _ = "SendProxySecretKey"
    formatMessage = messageName'
