{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, ScopedTypeVariables #-}

import Turtle
import Control.Monad.Trans.Maybe
import Control.Exception
import qualified Data.Text as T
import Safe
import System.Exit (exitWith)

data BuildkiteEnv = BuildkiteEnv
  { bkBuildNum :: Int
  , bkPipeline :: Text
  , bkBranch   :: Text
  } deriving (Show)

data CICacheConfig = CICacheConfig
  { ccMaxSize :: Maybe Text
  , ccBucket  :: Text
  , ccRegion  :: Maybe Text
  , ccPrefix  :: Text
  , ccBranch  :: Text
  } deriving (Show)

main :: IO ()
main = do
  awsCreds
  enableNixIntegration
  bk <- getBuildkiteEnv
  cacheConfig <- getCacheConfig bk
  cacheDownloadStep cacheConfig
  buildResult <- buildStep
  cacheUploadStep cacheConfig
  exitWith buildResult

buildStep :: IO ExitCode
buildStep = do
  echo "+++ Build and test"
  build .&&. test
  where
    cfg = ["--dump-logs", "--color", "always"]
    stackBuild args = run "stack" $ cfg ++ ["build", "--fast"] ++ args
    build = stackBuild ["--bench", "--no-run-benchmarks", "--no-haddock-deps"]
    test = stackBuild ["--test"]
    -- test = eprintf "Not executing tests because they are really really slow for some reason.\n" >> pure ExitSuccess

-- buildkite agents have S3 creds installed, but under different names
awsCreds :: IO ()
awsCreds = mapM_ (uncurry copy) things
  where
    copy src dst = need src >>= maybe (pure ()) (export dst)
    things = [ ( "BUILDKITE_S3_ACCESS_KEY_ID"
               , "AWS_ACCESS_KEY_ID")
             , ( "BUILDKITE_S3_SECRET_ACCESS_KEY"
               , "AWS_SECRET_ACCESS_KEY")
             , ( "BUILDKITE_S3_DEFAULT_REGION"
               , "AWS_DEFAULT_REGION")
             ]

-- cache-s3 runs stack to get paths, etc.
-- need to enable nix in /etc/stack/config.yaml.
-- in the meantime, do edit of the local stack config.
enableNixIntegration :: IO ()
enableNixIntegration = procs "sed" ["-i", "-e", "s/nix:/nix:\\n  enable: true/", "stack.yaml"] empty

getBuildkiteEnv :: IO (Maybe BuildkiteEnv)
getBuildkiteEnv = runMaybeT $ do
  bkBuildNum <- MaybeT $ needRead "BUILDKITE_BUILD_NUMBER"
  bkPipeline <- MaybeT $ need "BUILDKITE_PIPELINE_SLUG"
  bkBranch   <- MaybeT $ need "BUILDKITE_BRANCH"
  pure BuildkiteEnv{..}

needRead :: Read a => Text -> IO (Maybe a)
needRead v = (>>= readMay) . fmap T.unpack <$> need v

getCacheConfig :: Maybe BuildkiteEnv -> IO (Either Text CICacheConfig)
getCacheConfig Nothing  = pure (Left "BUILDKITE_* environment variables are not set")
getCacheConfig (Just BuildkiteEnv{..}) = do
  ccMaxSize <- need "CACHE_S3_MAX_SIZE"
  ccRegion <- need "AWS_REGION"
  need "S3_BUCKET" >>= \case
    Just ccBucket -> pure (Right CICacheConfig{ccBranch=bkBranch, ccPrefix=bkPipeline, ..})
    Nothing -> pure (Left "S3_BUCKET environment variable is not set")

cacheDownloadStep :: Either Text CICacheConfig -> IO ()
cacheDownloadStep cacheConfig = do
  echo "--- CI Cache Download"
  case cacheConfig of
    Right cfg -> restoreCICache cfg `catch`
      \ (ex :: IOException) -> do
        eprintf ("Failed to download CI cache: "%w%"\nContinuing anyway...\n") ex
    Left ex -> eprintf ("Not using CI cache because "%s%"\n") ex

cacheUploadStep :: Either Text CICacheConfig -> IO ()
cacheUploadStep cacheConfig = do
  echo "--- CI Cache Upload"
  case cacheConfig of
    Right cfg -> saveCICache cfg `catch`
      \ (ex :: IOException) -> do
        eprintf ("Failed to upload CI cache: "%w%"\n") ex
    Left _ -> printf "CI cache not configured.\n"

restoreCICache :: CICacheConfig -> IO ()
restoreCICache cfg = do
  cacheS3 cfg (Just "develop") "restore stack"
  cacheS3 cfg (Just "develop") "restore stack work"

saveCICache :: CICacheConfig -> IO ()
saveCICache cfg = do
  cacheS3 cfg Nothing "save stack"
  cacheS3 cfg Nothing "save stack work"

cacheS3 :: CICacheConfig -> Maybe Text -> Text -> IO ()
cacheS3 CICacheConfig{..} baseBranch cmd = void $ run "cache-s3" args
  where
    args = ml maxSize ++ ml regionArg ++
           [ format ("--bucket="%s) ccBucket
           , format ("--prefix="%s) ccPrefix
           , format ("--git-branch="%s) ccBranch
           , "--suffix=linux"
           , "-v"
           , "info"
           ] ++ cmds ++ ml baseBranchArg
    baseBranchArg = format ("--base-branch="%s) <$> baseBranch
    maxSize = format ("--max-size="%s) <$> ccMaxSize
    regionArg = format ("--region="%s) <$> ccRegion
    cmds = ("-c":T.words cmd)
    ml = maybe [] pure

run :: Text -> [Text] -> IO ExitCode
run cmd args = do
  printf (s%" "%s%"\n") cmd (T.unwords args)
  res <- proc cmd args empty
  case res of
    ExitSuccess -> pure ()
    ExitFailure code -> eprintf ("error: Command exited with code "%d%"!\nContinuing...\n") code
  pure res
