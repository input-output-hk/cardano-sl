-- | Server part.

module Pos.Communication.Server
       ( serve
       ) where

import           Universum

import           Pos.Communication.Server.Block ()
import           Pos.WorkMode                   (WorkMode)

serve :: WorkMode m => m ()
serve = pure ()
