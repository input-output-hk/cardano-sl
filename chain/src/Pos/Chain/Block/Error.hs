-- | Types describing runtime errors related to Block processing.
{-# LANGUAGE DeriveGeneric #-}

module Pos.Chain.Block.Error
       ( RollbackException(..)
       , ApplyBlocksException(..)
       , VerifyBlocksException(..)
       ) where

import           Universum

import           Control.DeepSeq (NFData)
import           Control.Exception.Safe (Exception (..))
import           Data.Text.Lazy.Builder (Builder, fromText)
import           Formatting (bprint, stext, (%))
import           Formatting.Buildable (Buildable (..))
import           GHC.Generics (Generic)

import           Pos.Chain.Block.Union (HeaderHash)
import           Pos.Crypto (shortHashF)


-- | This function can be used to create a message when tip mismatch
-- is detected (usually between tip stored in DB and some other tip
-- received from somewhere).
tipMismatchMsg :: Text -> HeaderHash -> HeaderHash -> Builder
tipMismatchMsg action storedTip attemptedTip =
    bprint
        ("Can't "%stext%" block because of tip mismatch (stored is "
         %shortHashF%", attempted is "%shortHashF%")")
        action storedTip attemptedTip

data RollbackException = RollbackTipMismatch HeaderHash HeaderHash
    deriving (Show)

renderRollbackException :: RollbackException -> Builder
renderRollbackException = \case
    RollbackTipMismatch storedTip attemptedTip ->
        tipMismatchMsg "rollback" storedTip attemptedTip

instance Exception RollbackException where
    displayException = toString . pretty

instance Buildable RollbackException where
    build = renderRollbackException

data ApplyBlocksException
    = ApplyBlocksTipMismatch
        Text -- message
        HeaderHash -- stored tip
        HeaderHash -- attempted tip
    | ApplyBlocksVerifyFailure VerifyBlocksException
    | ApplyBlocksError Text -- other error (not covered by constructors above)
    deriving (Show, Generic)

instance NFData ApplyBlocksException

renderApplyBlocksException :: ApplyBlocksException -> Builder
renderApplyBlocksException = \case
    ApplyBlocksTipMismatch s tip attemptedTip ->
        tipMismatchMsg s tip attemptedTip
    ApplyBlocksVerifyFailure e -> renderVerifyBlocksException e
    ApplyBlocksError e -> fromText e

instance Exception ApplyBlocksException where
    displayException = toString . pretty

instance Buildable ApplyBlocksException where
    build = renderApplyBlocksException

data VerifyBlocksException
    = VerifyBlocksError Text
    deriving (Show, Generic)

instance Exception VerifyBlocksException where
    displayException = toString . pretty

instance Buildable VerifyBlocksException where
    build = renderVerifyBlocksException

instance NFData VerifyBlocksException

renderVerifyBlocksException :: VerifyBlocksException -> Builder
renderVerifyBlocksException = \case
    VerifyBlocksError t -> fromText t
