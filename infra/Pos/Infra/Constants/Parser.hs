{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Pos.Infra.Constants.Parser
       ( infraConstants
       ) where

import           Universum                  hiding (lift)

#ifdef DEV_MODE
import           System.IO.Unsafe           (unsafePerformIO)
#else
import           Language.Haskell.TH.Syntax (lift, runIO)
#endif

import           Pos.Infra.Constants.Type   (InfraConstants)
import           Pos.Util.Config            (cslConfigFilePath, unsafeReadConfig)

infraConstants :: InfraConstants
#ifdef DEV_MODE
infraConstants = unsafePerformIO (unsafeReadConfig =<< cslConfigFilePath)
{-# NOINLINE infraConstants #-}
#else
infraConstants = $(do
    x :: InfraConstants <- runIO (unsafeReadConfig =<< cslConfigFilePath)
    lift x)
#endif
