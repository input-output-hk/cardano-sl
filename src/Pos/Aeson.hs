{-# LANGUAGE CPP #-}

-- | Module for exposing JSON instances for Cardano types

module Pos.Aeson
       (
       ) where

#ifdef WITH_WEB
import           Pos.Aeson.Crypto        ()
import           Pos.Aeson.Types         ()
#ifdef WITH_WALLET
import           Pos.Aeson.ClientTypes   ()
#endif
#endif

import           Pos.Aeson.CompileConfig ()
