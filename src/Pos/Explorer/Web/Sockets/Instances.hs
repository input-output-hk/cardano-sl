
module Pos.Explorer.Web.Sockets.Instances where

import           Data.Aeson (FromJSON (..))
import           Pos.Types  (Address, ChainDifficulty)
import           Universum

instance FromJSON Address where
    -- TODO: (NB: default implementation also makes a trick)
    parseJSON = undefined

instance FromJSON ChainDifficulty where
    parseJSON = undefined
