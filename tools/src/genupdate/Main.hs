{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import           Universum

import qualified Codec.Archive.Tar as Tar
import           Crypto.Hash (Digest, SHA512, hashlazy)
import qualified Data.ByteString.Lazy as BSL
import           Data.List ((\\))
import qualified Data.Text.Lazy.IO as TL
import           Data.Version (showVersion)
import           Formatting (Format, format, mapf, text, (%))
import qualified NeatInterpolation as NI (text)
import           Options.Applicative (Parser, execParser, footerDoc, fullDesc, header, help, helper,
                                      info, infoOption, long, metavar, option, progDesc, short)
import           Options.Applicative.Types (readerAsk)
import           System.Exit (ExitCode (ExitFailure))
import           System.FilePath (normalise, takeFileName, (<.>), (</>))
import qualified System.PosixCompat as PosixCompat
import           System.Process (readProcess)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl (version)
import           Pos.Util (directory, ls, withTempDir)

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
    where
      textOption = option (toText <$> readerAsk)

getUpdateGenOptions :: IO UpdateGenOptions
getUpdateGenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc ("")
                 <> header "Cardano SL updates generator."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-genupdate-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [NI.text|
Command example:

  stack exec -- cardano-genupdate --old /tmp/app-v000 --new /tmp/app-v001 -o /tmp/app-update.tar

Both directories must have equal file structure (e.g. they must contain the same
files in the same subdirectories correspondingly), otherwise 'cardano-genupdate' will fail.

Please note that 'cardano-genupdate' uses 'bsdiff' program, so make sure 'bsdiff' is available in the PATH.|]

main :: IO ()
main = do
    UpdateGenOptions{..} <- getUpdateGenOptions
    createUpdate (toString oldDir)
                 (toString newDir)
                 (toString outputTar)

-- | Outputs a `FilePath` as a `LText`.
fp :: Format LText (String -> LText)
fp = mapf toLText text

createUpdate :: FilePath -> FilePath -> FilePath -> IO ()
createUpdate oldDir newDir updPath = do
    oldFiles <- ls oldDir
    newFiles <- ls newDir
    -- find directories and fail if there are any (we can't handle
    -- directories)
    do oldNotFiles <- filterM (fmap (not . PosixCompat.isRegularFile) . PosixCompat.getFileStatus . normalise) oldFiles
       newNotFiles <- filterM (fmap (not . PosixCompat.isRegularFile) . PosixCompat.getFileStatus . normalise) newFiles
       unless (null oldNotFiles && null newNotFiles) $ do
           unless (null oldNotFiles) $ do
               TL.putStrLn $ format (fp%" contains not-files:") oldDir
               for_ oldNotFiles $ TL.putStrLn . format ("  * "%fp)
           unless (null newNotFiles) $ do
               TL.putStrLn $ format (fp%" contains not-files:") newDir
               for_ newNotFiles $ TL.putStrLn . format ("  * "%fp)
           putText "Generation aborted."
           exitWith (ExitFailure 2)
    -- fail if lists of files are unequal
    do let notInOld = map takeFileName newFiles \\ map takeFileName oldFiles
       let notInNew = map takeFileName oldFiles \\ map takeFileName newFiles
       unless (null notInOld && null notInNew) $ do
           unless (null notInOld) $ do
               putText "these files are in the NEW dir but not in the OLD dir:"
               for_ notInOld $ TL.putStrLn . format ("  * "%fp)
           unless (null notInNew) $ do
               TL.putStr "these files are in the OLD dir but not in the NEW dir:"
               for_ notInNew $ TL.putStrLn . format ("  * "%fp)
           putText "Generation aborted."
           exitWith (ExitFailure 3)
    -- otherwise, for all files, generate hashes and a diff
    withTempDir (directory updPath) "temp" $ \tempDir -> do
      (manifest, bsdiffs) <-
          fmap (unzip . catMaybes) $
          forM oldFiles $ \f -> do
              let fname = takeFileName f
                  oldFile = oldDir </> fname
                  newFile = newDir </> fname
                  diffFile = tempDir </> (fname <.> "bsdiff")
              oldHash <- hashFile oldFile
              newHash <- hashFile newFile
              if oldHash == newHash
                  then return Nothing
                  else do
                      _ <- readProcess "bsdiff" [oldFile, newFile, diffFile] mempty
                      diffHash <- hashFile diffFile
                      return (Just (unwords [oldHash, newHash, diffHash],
                                    takeFileName diffFile))
      -- write the MANIFEST file
      writeFile (tempDir </> "MANIFEST") (unlines manifest)
      -- put diffs and a manifesto into a tar file
      Tar.create updPath tempDir ("MANIFEST" : bsdiffs)

hashLBS :: LByteString -> Text
hashLBS lbs = show $ hashSHA512 lbs
  where
    hashSHA512 :: LByteString -> Digest SHA512
    hashSHA512 = hashlazy

hashFile :: FilePath -> IO Text
hashFile f = hashLBS <$> BSL.readFile f
