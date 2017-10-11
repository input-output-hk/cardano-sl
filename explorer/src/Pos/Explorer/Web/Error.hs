{-# LANGUAGE TemplateHaskell #-}

-- | Types describing runtime errors related to Explorer

module Pos.Explorer.Web.Error
       ( ExplorerError (..)
       , _Internal
       ) where

import           Control.Lens        (makePrisms)
import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))
import           Universum

newtype ExplorerError =
    -- | Some internal error.
    Internal Text
    deriving (Show, Generic)

makePrisms ''ExplorerError

instance Exception ExplorerError

instance Buildable ExplorerError where
    build (Internal msg) = bprint ("Internal explorer error ("%stext%")") msg
