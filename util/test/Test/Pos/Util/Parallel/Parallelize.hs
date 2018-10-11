{-# LANGUAGE LambdaCase #-}
module Test.Pos.Util.Parallel.Parallelize
       ( parallelizeAllCores
       ) where

import           Universum

import           GHC.Conc (getNumProcessors, setNumCapabilities)
import           System.Environment (lookupEnv)

-- | `parallelizeAllCores` gets the number of processors on a machine and
-- sets the number of threads equal to the number of processors on the machine.
-- This setting can be overridden by the @NIX_BUILD_CORES@ environment variable.
parallelizeAllCores :: IO ()
parallelizeAllCores = getNumParallel >>= setNumCapabilities
  where
    getNumParallel = getNixBuildCores >>= \case
      Just n -> pure n
      Nothing -> getNumProcessors

-- | Look up the @NIX_BUILD_CORES@ environment variable, if it
-- exists. This is set by the Nix builder to control parallelism
-- within a single build jobs.
getNixBuildCores :: IO (Maybe Int)
getNixBuildCores = readBuildCores <$> lookupEnv "NIX_BUILD_CORES"
  where
    readBuildCores (Just str) = case readMaybe str of
      Just n | n > 0 -> Just n
      _              -> Nothing
    readBuildCores Nothing    = Nothing
