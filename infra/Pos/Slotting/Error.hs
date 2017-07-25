-- | Run-time errors in Slotting.

module Pos.Slotting.Error
       ( SlottingError (..)
       ) where

import           Control.Exception   (Exception (..))
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Universum

import           Pos.Core.Types      (SlotId, EpochIndex, slotIdF)
import           Pos.Exception       (cardanoExceptionFromException,
                                      cardanoExceptionToException)

-- | Type aggregating run-time errors related to Slotting.
data SlottingError 
    = SEUnknownSlotStart !SlotId
    | SEUnknownLastEpoch !EpochIndex
  deriving (Show, Typeable)

instance Buildable SlottingError where
    build (SEUnknownSlotStart slot) =
        bprint ("start of "%slotIdF%" is surprisingly unknown") slot
    build (SEUnknownLastEpoch epoch) =
        bprint ("epoch "%build%" is surprisingly unknown") epoch

instance Exception SlottingError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty
