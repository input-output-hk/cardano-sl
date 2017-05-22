module Main
  ( main
  ) where

import qualified Codec.Archive.Tar    as Tar
import qualified Control.Foldl        as Fold
import           Crypto.Hash          (Digest, SHA512, hashlazy)
import qualified Data.ByteString.Lazy as BSL
import           Data.List            ((\\))
import           Filesystem.Path      (filename)
import           Turtle               hiding (f, toText)
import           Turtle.Prelude       (stat)
import           Universum            hiding (FilePath, fold)

main :: IO ()
main = do
    args <- map (fromString . toString) <$> arguments
    case args of
        [oldDir, newDir, updPath] -> createUpdate oldDir newDir updPath
        _                         -> printHelp >> exit (ExitFailure 1)

printHelp :: IO ()
printHelp = putStrLn $ unlines [
    "genupdate.sh - Update generator",
    "",
    "Usage: genupdate.sh [OLD DIR] [NEW DIR] [OUTPUT TAR FILE]",
    "",
    "If there are any files in one of the dirs that are not contained",
    "in the other dir, genupdate.sh will fail.",
    "",
    "bsdiff has to be present in path."
    ]

createUpdate :: FilePath -> FilePath -> FilePath -> IO ()
createUpdate oldDir newDir updPath = sh $ do
    oldFiles <- fold (ls oldDir) Fold.list
    newFiles <- fold (ls newDir) Fold.list
    -- find directories and fail if there are any (we can't handle
    -- directories)
    do oldNotFiles <- filterM (fmap (not . isRegularFile) . stat) oldFiles
       newNotFiles <- filterM (fmap (not . isRegularFile) . stat) newFiles
       unless (null oldNotFiles && null newNotFiles) $ do
           unless (null oldNotFiles) $ do
               printf (fp%" contains not-files:\n") oldDir
               for_ oldNotFiles $ printf ("  * "%fp%"\n")
           unless (null newNotFiles) $ do
               printf (fp%" contains not-files:\n") newDir
               for_ newNotFiles $ printf ("  * "%fp%"\n")
           echo "Generation aborted."
           exit (ExitFailure 2)
    -- fail if lists of files are unequal
    do let notInOld = map filename newFiles \\ map filename oldFiles
       let notInNew = map filename oldFiles \\ map filename newFiles
       unless (null notInOld && null notInNew) $ do
           unless (null notInOld) $ do
               echo "these files are in the NEW dir but not in the OLD dir:"
               for_ notInOld $ printf ("  * "%fp%"\n")
           unless (null notInNew) $ do
               echo "these files are in the OLD dir but not in the NEW dir:"
               for_ notInNew $ printf ("  * "%fp%"\n")
           echo "Generation aborted."
           exit (ExitFailure 3)
    -- otherwise, for all files, generate hashes and a diff
    tempDir <- mktempdir (directory updPath) "temp"
    (manifest, bsdiffs) <-
        fmap (unzip . catMaybes) $
        forM oldFiles $ \f -> do
            let fname = filename f
                oldFile = oldDir </> fname
                newFile = newDir </> fname
                diffFile = tempDir </> (fname <.> "bsdiff")
            oldHash <- hashFile oldFile
            newHash <- hashFile newFile
            if oldHash == newHash
                then return Nothing
                else do
                    _ <- proc "bsdiff"
                             (map fpToText [oldFile, newFile, diffFile])
                             mempty
                    diffHash <- hashFile diffFile
                    return (Just (unwords [oldHash, newHash, diffHash],
                                  filename diffFile))
    -- write the MANIFEST file
    liftIO $ writeTextFile (tempDir </> "MANIFEST") (unlines manifest)
    -- put diffs and a manifesto into a tar file
    liftIO $ Tar.create (fpToString updPath)
                        (fpToString tempDir)
                        ("MANIFEST" : map fpToString bsdiffs)

hashLBS :: LByteString -> Text
hashLBS lbs = show $ hashSHA512 lbs
  where
    hashSHA512 :: LByteString -> Digest SHA512
    hashSHA512 = hashlazy

hashFile :: MonadIO m => FilePath -> m Text
hashFile f = liftIO $ hashLBS <$> BSL.readFile (fpToString f)

fpToString :: FilePath -> String
fpToString = toString . format fp

fpToText :: FilePath -> Text
fpToText = toText . format fp
