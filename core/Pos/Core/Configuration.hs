module Pos.Core.Configuration
       ( HasConfiguration
       , module Pos.Core.Configuration.BlockVersionData
       , module Pos.Core.Configuration.Core
       , module Pos.Core.Configuration.GeneratedGenesisData
       , module Pos.Core.Configuration.GenesisAvvmBalances
       , module Pos.Core.Configuration.GenesisDelegation
       , module Pos.Core.Configuration.GenesisHash
       , module Pos.Core.Configuration.Protocol
       , module Pos.Core.Configuration.SharedSeed
       , module Pos.Core.Configuration.SystemStart
       ) where

import           Pos.Core.Configuration.BlockVersionData
import           Pos.Core.Configuration.Core
import           Pos.Core.Configuration.GeneratedGenesisData
import           Pos.Core.Configuration.GenesisAvvmBalances
import           Pos.Core.Configuration.GenesisDelegation
import           Pos.Core.Configuration.GenesisHash
import           Pos.Core.Configuration.Protocol
import           Pos.Core.Configuration.SharedSeed
import           Pos.Core.Configuration.SystemStart

-- | Coarse catch-all configuration constraint for use by depending modules.
type HasConfiguration =
    ( HasCoreConfiguration
    , HasProtocolConstants
    , HasGenesisBlockVersionData
    , HasGenesisHash
    , HasSharedSeed
    , HasSystemStart
    , HasGeneratedGenesisData
    , HasGenesisDelegation
    , HasGenesisAvvmBalances
    )
