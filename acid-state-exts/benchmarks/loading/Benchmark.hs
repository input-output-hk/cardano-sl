
import Benchmark.Prelude
import Criterion.Main
import qualified Data.Acid as Acid
import qualified Benchmark.FileSystem as FS
import qualified Benchmark.Model as Model; import Benchmark.Model (Model)
import qualified System.Random as Random



main :: IO ()
main = do

  workingPath <- do
    workingPath <- FS.getTemporaryDirectory
    rndStr <- replicateM 16 $ Random.randomRIO ('a', 'z')
    return $ workingPath <> "acid-state" <> "benchmarks" <> "loading" <> FS.decodeString rndStr

  putStrLn $ "Working under the following temporary directory: " ++ FS.encodeString workingPath
  FS.removeTreeIfExists workingPath
  FS.createTree workingPath

  defaultMain =<< sequence
    [
      prepareBenchmarksGroup workingPath $ 100,
      prepareBenchmarksGroup workingPath $ 200,
      prepareBenchmarksGroup workingPath $ 300,
      prepareBenchmarksGroup workingPath $ 400
    ]



prepareBenchmarksGroup :: FS.FilePath -> Int -> IO Benchmark
prepareBenchmarksGroup workingPath size = do
  putStrLn $ "Preparing instances for size " ++ show size

  let
    workingPath' = workingPath <> (FS.decodeString $ show size)
    logsInstancePath = workingPath' <> "logs-instance"
    checkpointInstancePath = workingPath' <> "checkpoint-instance"

  FS.createTree logsInstancePath
  FS.createTree checkpointInstancePath

  putStrLn "Initializing"
  inst <- initialize checkpointInstancePath size

  putStrLn "Copying"
  FS.copy checkpointInstancePath logsInstancePath
  FS.removeFile $ logsInstancePath <> "open.lock"

  putStrLn "Checkpointing"
  Acid.createCheckpoint inst

  putStrLn "Closing"
  Acid.closeAcidState inst

  return $ bgroup (show size)
    [
      bench "From Logs" $ nfIO $
        load logsInstancePath >>= Acid.closeAcidState,
      bench "From Checkpoint" $ nfIO $
        load checkpointInstancePath >>= Acid.closeAcidState
    ]



load :: FS.FilePath -> IO (Acid.AcidState Model)
load path = Acid.openLocalStateFrom (FS.encodeString path) mempty

initialize :: FS.FilePath -> Int -> IO (Acid.AcidState Model)
initialize path size = do
  inst <- Acid.openLocalStateFrom (FS.encodeString path) mempty
  let values = replicate size $ replicate 100 $ replicate 100 1
  mapM_ (Acid.update inst . Model.Insert) values
  return inst


