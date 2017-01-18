-- | Types describing runtime errors related to Update System.

module Pos.Update.Error
       ( USError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))
import           Universum

data USError
    = USInternalError !Text
     -- ^ Something bad happened inside Update System.
    deriving (Show)

instance Exception USError

instance Buildable USError where
    build (USInternalError msg) =
        bprint ("internal error in Update System: "%stext) msg
