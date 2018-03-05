{-# LANGUAGE DeriveGeneric #-}

-- | Types describing runtime errors related to
-- wallet integration tests.

module Error
    ( IntegrationTestError (..)
    ) where

import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))
import           Universum

newtype IntegrationTestError =
    -- | Some internal error.
    Internal Text
    deriving (Show, Generic)

instance Exception IntegrationTestError

instance Buildable IntegrationTestError where
    build (Internal msg) = bprint ("Internal integration test error ("%stext%")") msg
