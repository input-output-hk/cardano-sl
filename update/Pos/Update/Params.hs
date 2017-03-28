module Pos.Update.Params
       ( UpdateParams(..)
       ) where

import           Universum

data UpdateParams = UpdateParams
    { -- | Path to update installer executable, downloaded by update system
      upUpdatePath    :: !FilePath
    , -- | If `True` then use installer update mechanism
      upUpdateWithPkg :: !Bool
    , -- | List of update server URLs
      upUpdateServers :: ![Text]
    }
    deriving (Show)
