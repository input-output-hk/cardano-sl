module Test.Pos.Util.Golden where

import           Universum

import           Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import           Data.FileEmbed (embedStringFile)
import qualified Data.List as List
import           Hedgehog (Gen, Group, Property, PropertyT, TestLimit,
                     discoverPrefix, forAll, property, withTests, (===))
import           Hedgehog.Internal.TH (TExpQ)
import           Language.Haskell.TH (ExpQ, Q, loc_filename, runIO)
import           Language.Haskell.TH.Syntax (qLocation)
import           System.Directory (canonicalizePath)
import           System.FilePath (takeDirectory, (</>))


discoverGolden :: TExpQ Group
discoverGolden = discoverPrefix "golden_"

eachOf :: (Show a) => TestLimit -> Gen a -> (a -> PropertyT IO ()) -> Property
eachOf testLimit things hasProperty =
  withTests testLimit . property $ forAll things >>= hasProperty

-- | A handy shortcut for embedding golden testing files
embedGoldenTest :: FilePath -> ExpQ
embedGoldenTest path =
    makeRelativeToTestDir ("golden/" <> path) >>= embedStringFile

goldenTestJSON :: (ToJSON a, HasCallStack) => a -> FilePath -> Property
goldenTestJSON x path = withFrozenCallStack $ do
    withTests 1 . property $ do
        goldenJSON <- liftIO $ BS.readFile path
        (LB.toStrict $ encode x) === goldenJSON

makeRelativeToTestDir :: FilePath -> Q FilePath
makeRelativeToTestDir rel = do
    loc <- qLocation
    fp  <- runIO $ canonicalizePath $ loc_filename loc
    case findTestDir fp of
        Nothing ->
            error $ "Couldn't find directory 'test' in path: " <> toText fp
        Just testDir -> pure $ testDir </> rel
  where
    findTestDir f =
        let dir = takeDirectory f
        in  if dir == f
                then Nothing
                else if "/test" `List.isSuffixOf` dir
                    then Just dir
                    else findTestDir dir

