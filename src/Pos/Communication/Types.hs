{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication.

module Pos.Communication.Types
       ( ResponseMode
         -- * Messages and socket state
       , module Export
       ) where

import           Control.TimeWarp.Rpc             (MessageName, messageName)
import           Data.Proxy                       (Proxy (..))

import           Pos.Communication.Types.State    (MutSocketState)
import qualified Pos.Communication.Types.SysStart as SysStart
import           Pos.DHT.Model                    (MonadResponseDHT)
import           Pos.WorkMode                     (WorkMode)

import           Pos.Communication.Types.Protocol as Export
import           Pos.Communication.Types.State    as Export
import           Pos.Communication.Types.SysStart as Export
import           Pos.Txp.Types.Communication      as Export

-- [CSL-447] Remove ResponseMode
-- | Constraint alias for 'WorkMode' with 'MonadResponseDHT'.
type ResponseMode ssc m =
    (WorkMode ssc m, MonadResponseDHT (MutSocketState ssc) m)
