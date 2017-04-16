-- |

module Pos.Binary.DB where

import           Pos.Binary.Class (Bi (..))
import           Pos.DB.Block.Aux (BlundLocation (..))

instance Bi BlundLocation
