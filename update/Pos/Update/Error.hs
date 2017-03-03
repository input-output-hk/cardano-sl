-- | Types describing runtime errors related to Update System.

module Pos.Update.Error
       ( USError (..)
       ) where

import           Control.Exception   (Exception (..))
import qualified Data.Text.Buildable (Buildable (..))
import           Formatting          (bprint, stext, (%))
import           Universum

import           Pos.Exception       (cardanoExceptionFromException,
                                      cardanoExceptionToException)

data USError
    = USInternalError !Text
     -- ^ Something bad happened inside Update System.
    deriving (Show)

instance Exception USError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

instance Buildable USError where
    build (USInternalError msg) =
        bprint ("internal error in Update System: "%stext) msg
