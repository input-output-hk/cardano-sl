-- | Types describing runtime errors related to Txp.

module Pos.Txp.Error
       ( TxpError (..)
       ) where

import           Control.Exception.Safe (Exception (..))
import           Formatting (bprint, stext, (%))
import qualified Formatting.Buildable
import           Universum

import           Pos.Exception (cardanoExceptionFromException,
                     cardanoExceptionToException)

data TxpError
    = TxpInternalError !Text
    -- ^ Something bad happened inside Txp
    deriving (Show)

instance Exception TxpError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

instance Buildable TxpError where
    build (TxpInternalError msg) =
        bprint ("internal error in Transaction processing: "%stext) msg
