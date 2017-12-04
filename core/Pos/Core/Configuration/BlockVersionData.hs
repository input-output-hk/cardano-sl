{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.BlockVersionData
       ( HasGenesisBlockVersionData
       , withGenesisBlockVersionData
       , genesisBlockVersionData
       ) where


import           Data.Reflection (Given (..), give)

import           Pos.Core.Update.Types (BlockVersionData)

type HasGenesisBlockVersionData = Given BlockVersionData

withGenesisBlockVersionData :: BlockVersionData -> (HasGenesisBlockVersionData => r) -> r
withGenesisBlockVersionData = give

genesisBlockVersionData :: HasGenesisBlockVersionData => BlockVersionData
genesisBlockVersionData = given
