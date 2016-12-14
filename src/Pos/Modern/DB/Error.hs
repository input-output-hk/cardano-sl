-- | Types describing runtime errors related to DB.

module Pos.Modern.DB.Error
       ( DBError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))
import           Universum

data DBError =
    -- | Structure of DB is malformed (e. g. data is inconsistent,
    -- something is missing, etc.)
    DBMalformed !Text
    deriving (Show)

instance Exception DBError

instance Buildable DBError where
    build (DBMalformed msg) = bprint ("malformed DB ("%stext%")") msg
