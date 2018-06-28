{-# LANGUAGE DeriveLift  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}

-- | Provides the git revision which this was compiled from.
--
-- Stack builds will have the `git` command available to run during
-- compilation.
--
-- Nix builds will inject the git revision into the executables after
-- compiling. If the git revision has changed but the sources have
-- not, then no haskell packages will be rebuilt, but the embedded git
-- revision will be updated.

module Pos.Util.CompileInfo
       ( CompileTimeInfo (..)
       , HasCompileInfo
       , compileInfo
       , withCompileInfo
       , gitRev
       ) where

import           Universum

import           Data.FileEmbed (dummySpaceWith)
import           Data.Reflection (Given (..), give, given)
import qualified Data.Text as T
import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))
import           Pos.Util.CompileInfoGit

-- | Data about the system that we want to retrieve in compile time.
data CompileTimeInfo = CompileTimeInfo
    { ctiGitRevision :: Text
    } deriving (Show)

instance Buildable CompileTimeInfo where
    build CompileTimeInfo{..} =
        bprint ("Compile time info: git revision '"%stext%"'") ctiGitRevision

type HasCompileInfo = Given CompileTimeInfo

compileInfo :: HasCompileInfo => CompileTimeInfo
compileInfo = given

withCompileInfo :: (HasCompileInfo => r) -> r
withCompileInfo = give (CompileTimeInfo gitRev)

gitRev :: Text
gitRev | gitRevEmbed /= zeroRev = gitRevEmbed
       | T.null fromGit         = zeroRev
       | otherwise              = fromGit
    where
        -- Git revision embedded after compilation using
        -- Data.FileEmbed.injectWith. If nothing has been injected,
        -- this will be filled with 0 characters.
        gitRevEmbed :: Text
        gitRevEmbed = decodeUtf8 $(dummySpaceWith "gitrev" 40)

        -- Git revision found during compilation by running git. If
        -- git could not be run, then this will be empty.
        fromGit = T.strip (fromString $(gitRevFromGit))

zeroRev :: Text
zeroRev = "0000000000000000000000000000000000000000"
