-- | Run-time errors in Slotting.

module Pos.Slotting.Error
       ( SlottingError (..)
       ) where

import           Control.Exception   (Exception (..))
import qualified Data.Text.Buildable
import           Formatting          (bprint, (%))
import           Universum

import           Pos.Core.Types      (SlotId, slotIdF)
import           Pos.Exception       (cardanoExceptionFromException,
                                      cardanoExceptionToException)


data SlottingError
    = SEUnknownSlotStart !SlotId
    | SEUnknownSlot
  deriving (Show, Typeable)

instance Buildable SlottingError where
    build (SEUnknownSlotStart slot) =
        bprint ("start of "%slotIdF%" is surprisingly unknown") slot
    build (SEUnknownSlot) =
        bprint ("slot is unknown")


instance Exception SlottingError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty
