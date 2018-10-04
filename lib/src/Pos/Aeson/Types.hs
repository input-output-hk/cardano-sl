{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Aeson.Types
       (
       ) where

import           Data.Aeson.TH (defaultOptions, deriveToJSON)

import           Pos.Web.Types (SscStage)

-- NOTE: some of these types are used on frontend (PureScript).
-- We are automatically deriving instances there and they are
-- compitable now with `deriveToJSON defaultOptions ''Y`.
-- If datatype is used on frontend, please use this instead of
-- any other way of deriving if possible.

deriveToJSON defaultOptions ''SscStage
