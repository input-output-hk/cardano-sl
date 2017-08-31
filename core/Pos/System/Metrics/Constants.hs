{-# LANGUAGE OverloadedStrings #-}
module Pos.System.Metrics.Constants (
    withCardanoNamespace
                                        ) where

import           Universum

cardanoNamespace :: Text
cardanoNamespace = "cardano"

withCardanoNamespace :: Text -> Text
withCardanoNamespace label = cardanoNamespace <> "." <> label
