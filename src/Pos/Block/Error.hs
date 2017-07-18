-- | Types describing runtime errors related to Block processing.

module Pos.Block.Error
       ( BlkError
       , RollbackException(..)
       ) where

import           Universum

import           Control.Exception    (Exception (..))
import qualified Data.Text            as Text
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
    RollbackTipMismatch storedTip attemptedTip->
        tipMismatchMsg "rollback" storedTip attemptedTip

instance Exception RollbackException where
    displayException = Text.unpack . renderRollbackException

instance Buildable RollbackException where
    build = Data.Text.Buildable.build . renderRollbackException
