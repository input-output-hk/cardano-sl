module Test.Pos.Util.Golden where

import           Universum

import           Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LB
import           Data.FileEmbed (embedStringFile)
import qualified Data.List as List
import           Data.SafeCopy (SafeCopy, safeGet, safePut)
import           Data.Serialize (runGetLazy, runPutLazy)
import qualified Data.Text.Lazy as LT (unpack)
import           Data.Text.Lazy.Builder (toLazyText)
import           Formatting.Buildable (build)
import           Hedgehog (Gen, Group, Property, PropertyT, TestLimit,
                     discoverPrefix, forAll, property, withTests, (===))
import           Hedgehog.Internal.Property (failWith)
import           Hedgehog.Internal.TH (TExpQ)
import           Language.Haskell.TH (ExpQ, Q, loc_filename, runIO)
import           Language.Haskell.TH.Syntax (qLocation)
import           System.Directory (canonicalizePath)
import           System.FilePath (takeDirectory, (</>))
import qualified Text.JSON.Canonical as Canonical

import           Pos.Util.Json.Canonical (SchemaError (..))

discoverGolden :: TExpQ Group
discoverGolden = discoverPrefix "golden_"

eachOf :: (Show a) => TestLimit -> Gen a -> (a -> PropertyT IO ()) -> Property
eachOf testLimit things hasProperty =
  withTests testLimit . property $ forAll things >>= hasProperty

-- | A handy shortcut for embedding golden testing files
embedGoldenTest :: FilePath -> ExpQ
embedGoldenTest path =
    makeRelativeToTestDir ("golden/" <> path) >>= embedStringFile

goldenTestJSON :: (Eq a, FromJSON a, HasCallStack, Show a, ToJSON a)
               => a -> FilePath -> Property
goldenTestJSON x path = withFrozenCallStack $ do
    withTests 1 . property $ do
        bs <- liftIO (LB.readFile path)
        encode x === bs
        case eitherDecode bs of
            Left err -> failWith Nothing $ "could not decode: " <> show err
            Right x' -> x === x'

goldenTestJSONPretty :: (Eq a, FromJSON a, HasCallStack, Show a, ToJSON a)
               => a -> FilePath -> Property
goldenTestJSONPretty x path = withFrozenCallStack $ do
    withTests 1 . property $ do
        bs <- liftIO (LB.readFile path)
        encodePretty x === bs
        case eitherDecode bs of
            Left err -> failWith Nothing $ "could not decode: " <> show err
            Right x' -> x === x'

-- | Only check that the datatype equals the decoding of the file
goldenTestJSONDec :: (Eq a, FromJSON a, HasCallStack, Show a)
                  => a -> FilePath -> Property
goldenTestJSONDec x path = withFrozenCallStack $ do
    withTests 1 . property $ do
        bs <- liftIO (LB.readFile path)
        case eitherDecode bs of
            Left err -> failWith Nothing $ "could not decode: " <> show err
            Right x' -> x === x'

goldenTestCanonicalJSONDec
    :: ( Eq a
       , Canonical.FromJSON (Either SchemaError) a
       , HasCallStack
       , Show a
       )
    => a
    -> FilePath
    -> Property
goldenTestCanonicalJSONDec x path = withFrozenCallStack $ do
    withTests 1 . property $ do
        bs <- liftIO (LB.readFile path)
        case Canonical.parseCanonicalJSON bs of
             Left err  -> failWith Nothing $ "could not parse: " <> show err
             Right jsv -> case Canonical.fromJSON jsv of
                Left (schErr :: SchemaError) ->
                    failWith Nothing $ LT.unpack $ toLazyText $ build schErr
                Right x'    -> x === x'

goldenTestSafeCopy :: (Eq a, SafeCopy a, HasCallStack, Show a)
               => a -> FilePath -> Property
goldenTestSafeCopy x path = withFrozenCallStack $ do
    withTests 1 . property $ do
        bs <- liftIO (LB.readFile path)
        runPutLazy (safePut x) === bs
        case runGetLazy safeGet bs of
            Left err -> failWith Nothing $ "could not safeGet: " <> show err
            Right x' -> x === x'

goldenTestSafeCopyDec :: (Eq a, SafeCopy a, HasCallStack, Show a)
               => a -> FilePath -> Property
goldenTestSafeCopyDec x path = withFrozenCallStack $ do
    withTests 1 . property $ do
        bs <- liftIO (LB.readFile path)
        case runGetLazy safeGet bs of
            Left err -> failWith Nothing $ "could not safeGet: " <> show err
            Right x' -> x === x'

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

