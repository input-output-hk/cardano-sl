-- | Types describing runtime errors related to Block processing.

module Pos.Block.Error
       ( BlkError
       , RollbackException(..)
       , ApplyBlocksException(..)
       , VerifyBlocksException(..)
       ) where

import           Universum

import           Control.Exception    (Exception (..))
import qualified Data.Text.Buildable

import           Pos.Block.Logic.Util (tipMismatchMsg)
import           Pos.Core             (HeaderHash)

data BlkError

--instance Exception BlkError

--instance Buildable BlkError

data RollbackException = RollbackTipMismatch HeaderHash HeaderHash
    deriving (Show)

renderRollbackException :: RollbackException -> Text
renderRollbackException = \case
    RollbackTipMismatch storedTip attemptedTip ->
        tipMismatchMsg "rollback" storedTip attemptedTip

instance Exception RollbackException where
    displayException = toString . renderRollbackException

instance Buildable RollbackException where
    build = Data.Text.Buildable.build . renderRollbackException

data ApplyBlocksException
    = ApplyBlocksTipMismatch Text HeaderHash HeaderHash
    | ApplyBlocksVerifyFailure VerifyBlocksException
    | ApplyBlocksError Text
    deriving (Show)

renderApplyBlocksException :: ApplyBlocksException -> Text
renderApplyBlocksException = \case
    ApplyBlocksTipMismatch s tip attemptedTip ->
        tipMismatchMsg s tip attemptedTip
    ApplyBlocksVerifyFailure e -> renderVerifyBlocksException e
    ApplyBlocksError e -> e

instance Exception ApplyBlocksException where
    displayException = toString . renderApplyBlocksException

instance Buildable ApplyBlocksException where
    build = Data.Text.Buildable.build . renderApplyBlocksException

data VerifyBlocksException
    = VerifyBlocksError Text
    deriving (Show)

instance Exception VerifyBlocksException where
    displayException = toString . renderVerifyBlocksException

instance Buildable VerifyBlocksException where
    build = Data.Text.Buildable.build . renderVerifyBlocksException

renderVerifyBlocksException :: VerifyBlocksException -> Text
renderVerifyBlocksException = \case
    VerifyBlocksError t -> t
