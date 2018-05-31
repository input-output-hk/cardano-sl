{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Crypto.TempHelpers
       ( goldenTestBi
       , embedGoldenTest
       , discoverGolden
       , discoverRoundTrip
       , runTests
       , roundTripsBiBuildable
       , trippingBiShow
       , compareHexDump
       , eachOf
       ) where

-- These helpers really belong in the `binary` package and then be exported via
-- a `cardano-sl-binary-test` package, but current issues with path lengths in
-- AppVeyor CI (for Windows only) means that they can't be put there.
-- We therefore cargo cult this module to wherever we need it and when the
-- Windows build issues are fixed we then remove this module and do things
-- the right way.

-- When moved to the `binary` package this module should probably be called
-- something like `Test.Pos.Binary.Helpers`.




import           Universum

import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.FileEmbed (embedStringFile)
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Text.Buildable (Buildable (..))
import           Data.Text.Internal.Builder (fromText, toLazyText)
import qualified Data.Text.Lazy as LazyText
import           Language.Haskell.TH (ExpQ, Q, loc_filename, runIO)
import           Language.Haskell.TH.Syntax (qLocation)
import           System.Directory (canonicalizePath)
import           System.FilePath (takeDirectory, (</>))

import           Hedgehog (Gen, Group, MonadTest, Property, PropertyT, discoverPrefix, eval, forAll,
                           property, success, tripping, withTests, (===))
import           Hedgehog.Internal.Property (Diff (..), failWith)
import           Hedgehog.Internal.Show (LineDiff, lineDiff, mkValue, renderLineDiff, showPretty,
                                         valueDiff)
import           Hedgehog.Internal.TH (TExpQ)

import           Pos.Binary.Class (Bi (..), decodeFull, serialize)

import qualified Prelude

import           Text.Show.Pretty (Value (..), parseValue)

import qualified Test.Pos.Util.Base16 as B16

runTests :: [IO Bool] -> IO ()
runTests tests = do
    result <- and <$> sequence tests
    unless result
        exitFailure

type HexDump = LByteString

type HexDumpDiff = [LineDiff]

renderHexDumpDiff :: HexDumpDiff -> String
renderHexDumpDiff = Prelude.unlines . fmap renderLineDiff

-- | Diff two @HexDump@s by comparing lines pairwise
hexDumpDiff :: HexDump -> HexDump -> Maybe HexDumpDiff
hexDumpDiff x y =
  concatMap (uncurry lineDiff)
    ... zipWithPadding (String "") (String "")
    <$> (sequence $ mkValue <$> BS.lines x)
    <*> (sequence $ mkValue <$> BS.lines y)

zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

-- | A custom version of @(===)@ for @HexDump@s to get prettier diffs
compareHexDump :: (MonadTest m, HasCallStack) => HexDump -> HexDump -> m ()
compareHexDump x y = do
    ok <- withFrozenCallStack $ eval (x == y)
    if ok then success else withFrozenCallStack $ failHexDumpDiff x y

-- | Fail with a nice line diff of the two HexDumps
failHexDumpDiff :: (MonadTest m, HasCallStack) => HexDump -> HexDump -> m ()
failHexDumpDiff x y =
  case hexDumpDiff x y of
    Nothing ->
      withFrozenCallStack $
        failWith Nothing $ Prelude.unlines [
            "━━━ Not Equal ━━━"
          , showPretty x
          , showPretty y
          ]
    Just diff ->
      withFrozenCallStack $ failWith Nothing $ renderHexDumpDiff diff

makeRelativeToTestDir :: FilePath -> Q FilePath
makeRelativeToTestDir rel = do
    loc <- qLocation
    fp  <- runIO $ canonicalizePath $ loc_filename loc
    case findTestDir fp of
        Nothing ->
            error $ "Couldn't find directory 'test' in path: " <> Text.pack fp
        Just testDir -> pure $ testDir </> rel
  where
    findTestDir f =
        let dir = takeDirectory f
        in  if dir == f
                then Nothing
                else if List.isSuffixOf "/test" dir
                    then Just dir
                    else findTestDir dir

-- | A handy shortcut for embedding golden testing files
embedGoldenTest :: FilePath -> ExpQ
embedGoldenTest path =
    makeRelativeToTestDir ("golden/" <> path) >>= embedStringFile

discoverGolden :: TExpQ Group
discoverGolden = discoverPrefix "golden_"

discoverRoundTrip :: TExpQ Group
discoverRoundTrip = discoverPrefix "roundTrip"

goldenTestBi :: (Bi a, Eq a, Show a, HasCallStack) => a -> FilePath -> Property
goldenTestBi x path = withFrozenCallStack $ do
    let bs' = B16.encodeWithIndex . serialize $ x
    withTests 1 . property $ do
        bs <- liftIO $ BS.readFile path
        let target = B16.decode bs
        compareHexDump bs bs'
        fmap decodeFull target === Just (Right x)

eachOf :: (Show a) => Gen a -> (a -> PropertyT IO ()) -> Property
eachOf things hasProperty =
  withTests 1000 . property $ forAll things >>= hasProperty

-- | Round trip test a value (any instance of both the 'Bi' and 'Show' classes)
-- by serializing it to a ByteString and back again and
--   that also has a 'Show' instance.
-- If the 'a' type has both 'Show' and 'Buildable' instances, its best to
-- use this version.
trippingBiShow :: (Bi a, Eq a, MonadTest m, Show a) => a -> m ()
trippingBiShow x =
    tripping x serialize decodeFull

-- | Round trip (via ByteString) any instance of the 'Bi' class
-- that also has a 'Buildable' instance.
roundTripsBiBuildable :: (Bi a, Buildable a, Eq a, MonadTest m) => a -> m ()
roundTripsBiBuildable x =
  let mx = pure x
      i = serialize x
      my = decodeFull i
  in if mx == my
        then success
        else case valueDiff <$> buildValue mx <*> buildValue my of
            Nothing ->
                withFrozenCallStack $
                    failWith Nothing $ Prelude.unlines
                        [ "━━━ Original ━━━"
                        , buildPretty mx
                        , "━━━ Intermediate ━━━"
                        , BS.unpack i
                        , "━━━ Roundtrip ━━━"
                        , buildPretty my
                        ]

            Just diff ->
                withFrozenCallStack $
                    failWith
                        (Just $ Diff "━━━ " "- Original" "/" "+ Roundtrip" " ━━━" diff) $
                            Prelude.unlines
                            [ "━━━ Intermediate ━━━"
                            , BS.unpack i
                            ]

instance Buildable a => Buildable (Either Text a) where
    build (Left t)  = fromText t
    build (Right a) = build a

buildPretty :: Buildable a => a -> String
buildPretty = show . buildValue

buildValue :: Buildable a => a -> Maybe Value
buildValue = parseValue . stringBuild

stringBuild :: Buildable a => a -> String
stringBuild =
    LazyText.unpack . toLazyText  . build
