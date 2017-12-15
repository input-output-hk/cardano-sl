-- | Custom JSON serialisation options

module Pos.Wallet.Aeson.Options
    ( customOptions
    , customOptionsWithTag
    ) where

import           Universum

import           Data.Aeson.TH (Options (..), SumEncoding (..), defaultOptions, defaultTaggedObject)

-- Let's use something similar to this options one day
customOptions :: Options
customOptions =
    defaultOptions
    { -- avoid encoding to plain string for enumeration types
      allNullaryToStringTag = False
      -- allows to make newtypes trasparent for serialization
    , unwrapUnaryRecords = True
    }

customOptionsWithTag :: String -> Options
customOptionsWithTag tag =
    customOptions
    { sumEncoding =
        defaultTaggedObject
        { tagFieldName = tag
        }
    }
