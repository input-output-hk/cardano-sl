module Pos.Core.Version () where

import           Data.Text.Buildable (Buildable)
import           Formatting          (bprint, shown)
import           Prelude             (show)
import           Universum           hiding (show)

import qualified Data.Text.Buildable as Buildable
import           Pos.Core.Types      (BlockVersion (..))

instance Show BlockVersion where
    show BlockVersion {..} =
        intercalate "." [show bvMajor, show bvMinor, show bvAlt]

instance Buildable BlockVersion where
    build = bprint shown
