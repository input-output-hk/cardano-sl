-- | Errors which can happen during blockchain generation.

module Pos.Generator.Block.Error
       ( BlockGenError (..)
       ) where

import           Universum

import           Control.Exception.Safe (Exception (..))
import qualified Data.Text.Buildable
import           Formatting (bprint, build, stext, (%))

import           Pos.Binary.Core ()
import           Pos.Block.Error (VerifyBlocksException)
import           Pos.Core (Address, StakeholderId, addressF)
import           Pos.Crypto (shortHashF)
import           Pos.Exception (cardanoExceptionFromException, cardanoExceptionToException)

-- | Errors which can happen during blockchain generation.
data BlockGenError
    = BGUnknownSecret !StakeholderId
    -- ^ Generator needs secret key of given stakeholder, but it can't
    -- be found in the context.
    | BGUnknownAddress !Address
    -- ^ Generator needs spending data of given address, but it can't
    -- be found in the context.
    | BGFailedToCreate !Text
    -- ^ Block generator failed to create a block.
    | BGCreatedInvalid !VerifyBlocksException
    -- ^ Block generator created invalid block.
    | BGInternal !Text
    -- ^ Internal error occurred.
    deriving (Show)

instance Buildable BlockGenError where
    build (BGUnknownSecret sId) =
        bprint
            ("Secret key of "%shortHashF%" is required but isn't known") sId
    build (BGUnknownAddress addr) =
        bprint
            ("Spending data of "%addressF%" is required but isn't known") addr
    build (BGFailedToCreate reason) =
        bprint ("Failed to create a block: "%stext) reason
    build (BGCreatedInvalid reason) =
        bprint ("Unfortunately, I created invalid block: "%build) reason
    build (BGInternal reason) =
        bprint ("Internal error: "%stext) reason

instance Exception BlockGenError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty
