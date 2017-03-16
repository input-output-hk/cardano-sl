-- | Types describing runtime errors related to Explorer

module Pos.Explorer.Socket.Error
       ( NotifierError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))
import           Universum

data NotifierError =
    -- | Some internal error.
    InternalError !Text
    deriving (Show, Generic)

instance Exception NotifierError

instance Buildable NotifierError where
    build (InternalError msg) =
        bprint ("Internal notifier error ("%stext%")") msg
