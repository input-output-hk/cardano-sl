module Explorer.View.Dashboard.Types where

import Data.Maybe (Maybe)
import Explorer.Types.Actions (Action)


newtype HeaderOptions = HeaderOptions
    { headline :: String
    , link :: Maybe HeaderLink
    }

newtype HeaderLink = HeaderLink
    { label :: String
    , action :: Action
    }
