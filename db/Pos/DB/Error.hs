-- | Types describing runtime errors related to DB.

module Pos.DB.Error
       ( DBError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting (bprint, int, stext, (%))
import           Universum

data DBError =
      -- | Structure of DB is malformed (e. g. data is inconsistent,
      -- something is missing, etc.)
      DBMalformed !Text
    | DBUnexpectedVersionTag !Word8 !Word8 -- ^ The first field is the expected version
                                           -- tag. The second is the one received.
    deriving (Show)

instance Exception DBError
-- TODO Make it cardanoException

instance Buildable DBError where
    build (DBMalformed msg) = bprint ("malformed DB ("%stext%")") msg
    build (DBUnexpectedVersionTag w1 w2) =
        bprint ("unexpected version tag (Expected version tag: "%int%". Got: "%int%")")
               w1
               w2
