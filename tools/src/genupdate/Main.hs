{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main
  ) where

import qualified Codec.Archive.Tar            as Tar
import qualified Control.Foldl                as Fold
import           Crypto.Hash                  (Digest, SHA512, hashlazy)
import qualified Data.ByteString.Lazy         as BSL
import           Data.List                    ((\\))
import           Data.String.QQ               (s)
import           Data.Version                 (showVersion)
import           Filesystem.Path              (filename)
import           Options.Applicative.Simple   (Parser, execParser, footerDoc, fullDesc,
                                               help, helper, info, infoOption, long,
                                               metavar, progDesc, short)
import qualified Options.Applicative.Simple   as S
import           Options.Applicative.Text     (textOption)
import           Paths_cardano_sl             (version)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Turtle                       hiding (f, s, toText)
import           Turtle.Prelude               (stat)
import           Universum                    hiding (FilePath, fold)

data UpdateGenOptions = UpdateGenOptions
    { oldDir    :: !Text
    , newDir    :: !Text
    , outputTar :: !Text
    } deriving (Show)

optionsParser :: Parser UpdateGenOptions
optionsParser = do
    oldDir <- textOption $
           long    "old"
        <> metavar "PATH"
        <> help    "Path to directory with old program."
    newDir <- textOption $
           long    "new"
        <> metavar "PATH"
        <> help    "Path to directory with new program."
    outputTar <- textOption $
           short   'o'
        <> long    "output"
        <> metavar "PATH"
        <> help    "Path to output .tar-file with diff."
    pure UpdateGenOptions{..}

getUpdateGenOptions :: IO UpdateGenOptions
getUpdateGenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc ("")
                 <> S.header "Cardano SL updates generator."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-genupdate-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = Just [s|
Command example:

  stack exec -- cardano-genupdate --old /tmp/app-v000 --new /tmp/app-v001 -o /tmp/app-update.tar

Both directories must have equal file structure (e.g. they must contain the same
files in the same subdirectories correspondingly), otherwise 'cardano-genupdate' will fail.

Please note that 'cardano-genupdate' uses 'bsdiff' program, so make sure 'bsdiff' is available in the PATH.|]

main :: IO ()
main = do
    UpdateGenOptions{..} <- getUpdateGenOptions
    createUpdate (fromText oldDir)
                 (fromText newDir)
                 (fromText outputTar)

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
