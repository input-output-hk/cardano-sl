{-# LANGUAGE DeriveLift  #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}

-- | Compile time information manipulations. Was introduced as
-- CSL-1563 to avoid extra library recompilations when git revision
-- changes. See the issue description/comments for more details.

module Pos.Util.CompileInfo
       ( CompileTimeInfo (..)
       , HasCompileInfo
       , compileInfo
       , withCompileInfo
       , retrieveCompileTimeInfo
       ) where

import           Universum

import           Data.Default (Default (def))
import           Data.Reflection (Given (..), give, given)
import qualified Data.Text as T
import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))
import           Instances.TH.Lift ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode (..))
import           System.Process (readProcessWithExitCode)


-- | Data about the system that we want to retrieve in compile time.
data CompileTimeInfo = CompileTimeInfo
    { ctiGitRevision :: Text
    } deriving (Show,TH.Lift)

instance Default CompileTimeInfo where
    def = CompileTimeInfo { ctiGitRevision = "<def instance>"
                          }

instance Buildable CompileTimeInfo where
    build CompileTimeInfo{..} =
        bprint ("Compile time info: git revision '"%stext%"'") ctiGitRevision

type HasCompileInfo = Given CompileTimeInfo

compileInfo :: HasCompileInfo => CompileTimeInfo
compileInfo = given

withCompileInfo :: CompileTimeInfo -> (HasCompileInfo => r) -> r
withCompileInfo = give

retrieveCompileTimeInfo :: TH.Q TH.Exp
retrieveCompileTimeInfo = do
    cti <- TH.runIO $ do
      ctiGitRevision <- T.strip . fromString <$> retrieveGit
      pure $ CompileTimeInfo {..}
    TH.lift cti
  where
    retrieveGit :: IO String
    retrieveGit =
        lookupEnv "GITREV" >>= maybe retrieveFromGitExecutable pure
    retrieveFromGitExecutable :: IO String
    retrieveFromGitExecutable = do
        (exitCode, output, _) <-
            readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
        pure $ case exitCode of
            ExitSuccess -> output
            _           -> "Couldn't fetch git revision"
