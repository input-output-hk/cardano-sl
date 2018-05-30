module Pos.Core.Ssc.Opening
       ( Opening (..)
       ) where

import           Universum

import           Pos.Binary.Class (AsBinary)
import           Pos.Crypto (Secret)

-- | Opening reveals secret.
newtype Opening = Opening
    { getOpening :: AsBinary Secret
    } deriving (Show, Eq, Generic, Buildable, NFData)
