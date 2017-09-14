-- | This module contains core constants with more descriptive types
-- (comparing to 'Pos.Core.Constants.Raw').

module Pos.Core.Constants.Typed
       (
         staticSysStart
       , staticBlkSecurityParam
       ) where

import           Universum

import           Pos.Core.Constants.Raw (CoreConfig (..), coreConfig, staticSysStartRaw)
import           Pos.Core.Types         (BlockCount, Timestamp (..))

----------------------------------------------------------------------------
-- Constants taken from the config
----------------------------------------------------------------------------

-- | System start time embedded into binary.
staticSysStart :: Timestamp
staticSysStart = Timestamp staticSysStartRaw

-- | Security parameter which is maximum number of blocks which can be
-- rolled back. This value is embedded into library and can be used
-- only for initialization. The actual value should be fetched from
-- runtime context (it can differ from this one).
staticBlkSecurityParam :: BlockCount
staticBlkSecurityParam = fromIntegral $ ccK coreConfig
