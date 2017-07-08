{-# LANGUAGE TypeApplications #-}

-- | This is a separate module due to the TH stage restriction
module Pos.Util.Config.Contents
       ( cslConfigFile
       ) where

import           Pos.Util.Config.Path
import           Universum                  hiding (lift)
import           Language.Haskell.TH.Syntax (lift, runIO, addDependentFile)

-- | config that should be used by all parts of Cardano SL.
--
-- The contents of the config is @constants.yaml@.
cslConfigFile :: String
cslConfigFile = $(do
    addDependentFile cslConfigFilePath
    contents <- runIO (readFile cslConfigFilePath)
    lift $ toString contents
  )
