{-# LANGUAGE RankNTypes #-}

-- | Compile time information manipulations. Was introduced as
-- CSL-1563 to avoid extra library recompilations when git revision
-- changes. See the issue description/comments for more details.

module Pos.Util.CompileInfo
       ( CompileTimeInfo (..)
       , withCompileInfo
       , retrieveCompileTimeInfo
       ) where

import           Universum

import           Data.Reflection     (Given (..), give)
import           Language.Haskell.TH (Q)


-- | Data about the system that we want to retrieve in compile time.
data CompileTimeInfo = CompileTimeInfo
    { gtiGitRevision :: Text
    } deriving (Show)

type HasCompileInfo = Given CompileTimeInfo

withCompileInfo :: CompileTimeInfo -> (HasCompileInfo => r) -> r
withCompileInfo = give

retrieveCompileTimeInfo :: Q CompileTimeInfo
retrieveCompileTimeInfo = undefined
