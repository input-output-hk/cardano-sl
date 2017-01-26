{-# LANGUAGE ExistentialQuantification #-}

-- | Exceptions hierarchy in cardano-sl.

module Pos.Exception
       ( CardanoException (..)
       , cardanoExceptionToException
       , cardanoExceptionFromException

       , CardanoFatalError (..)
       ) where

import           Control.Exception   (Exception (..))
import qualified Data.Text.Buildable
import           Data.Typeable       (cast)
import           Formatting          (bprint, stext, (%))
import qualified Text.Show
import           Universum

-- | Root of exceptions in cardano-sl.
data CardanoException =
    forall e. (Buildable e, Exception e) =>
              CardanoException e
    deriving (Typeable)

instance Show CardanoException where
    show (CardanoException e) = toString . pretty $ e

instance Exception CardanoException

-- | Helper to define sub-exception of CardanoException.
cardanoExceptionToException :: (Buildable e, Exception e) => e -> SomeException
cardanoExceptionToException = toException . CardanoException

-- | Helper to define sub-exception of CardanoException.
cardanoExceptionFromException :: Exception e => SomeException -> Maybe e
cardanoExceptionFromException x = do
    CardanoException a <- fromException x
    cast a

-- | Error indicating that something really bad happened.
data CardanoFatalError =
    CardanoFatalError !Text
    deriving (Typeable, Show)

instance Buildable CardanoFatalError where
    build (CardanoFatalError msg) =
        bprint ("something really bad happened: "%stext) msg

instance Exception CardanoFatalError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty
