{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Aeson Orphans. |-}

module Cardano.Wallet.Orphans.Aeson where

import           Data.Aeson (ToJSON (..))
import           Pos.Wallet.Web.ClientTypes.Types (CFilePath (..))

instance ToJSON CFilePath where
  toJSON (CFilePath c) = toJSON c
