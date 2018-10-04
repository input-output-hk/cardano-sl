module Pos.System.Metrics.Constants (
      cardanoNamespace
    , withCardanoNamespace
                                        ) where

import           Universum

cardanoNamespace :: Text
cardanoNamespace = "cardano"

withCardanoNamespace :: Text -> Text
withCardanoNamespace label = cardanoNamespace <> "." <> label
