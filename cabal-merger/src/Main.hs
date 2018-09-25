module Main (main) where

import           Data.Either (fromRight)
import           Data.Foldable (foldlM)
import           Data.List (isPrefixOf)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.License (License (MIT))
import           Distribution.ModuleName (ModuleName, toFilePath)
import           Distribution.Package (Dependency (Dependency),
                     PackageIdentifier (PackageIdentifier, pkgName, pkgVersion),
                     PackageName, depPkgName, mkPackageName)
import           Distribution.PackageDescription (BuildInfo (buildable, defaultExtensions, defaultLanguage, hsSourceDirs, otherModules, targetBuildDepends),
                     BuildType (Simple),
                     CondTree (CondNode, condTreeComponents, condTreeConstraints, condTreeData),
                     ConfVar, Executable (buildInfo, Executable, modulePath),
                     Library (exposedModules, libBuildInfo),
                     PackageDescription (buildTypeRaw, homepage, licenseRaw, maintainer, package, specVersionRaw),
                     emptyBuildInfo, emptyLibrary, emptyPackageDescription,
                     executables)
import Distribution.Types.TestSuite
import           Distribution.PackageDescription.Parsec
                     (readGenericPackageDescription)
import           Distribution.PackageDescription.PrettyPrint
                     (writeGenericPackageDescription)
import           Distribution.Types.GenericPackageDescription (GenericPackageDescription (condExecutables, condLibrary, packageDescription, condTestSuites),
                     emptyGenericPackageDescription)
import           Distribution.Types.UnqualComponentName (UnqualComponentName)
import           Distribution.Verbosity (silent)
import           Distribution.Version (anyVersion, mkVersion, orLaterVersion,
                     thisVersion, unionVersionRanges)
import           Filesystem.Path (directory)
import           Filesystem.Path.CurrentOS (fromText, toText)
import           Language.Haskell.Extension (Extension, Language (Haskell2010))
import           System.Environment (getArgs)

data State =
  State {
    sExposedModules  :: [ ModuleName ]
  , sLibDepends      :: S.Set Dependency
  , sNamesToExclude  :: [ PackageName ]
  , sLibExtensions   :: S.Set Extension
  , sLibOtherModules :: S.Set ModuleName
  , sExecutables     :: [ Executable ]
  , sCondExecutables :: [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
  , sConfTestSuites  :: [ (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite) ]
  } deriving Show

instance Ord Dependency where
  compare a b = compare (depPkgName a) (depPkgName b)

fixExecutableSrc :: String -> Executable -> Executable
fixExecutableSrc prefix exe = exe {
    buildInfo = (buildInfo exe) {
        hsSourceDirs = map (prefix <>) (hsSourceDirs $ buildInfo exe)
    }
  }

goExecutable :: String -> State -> (UnqualComponentName, CondTree ConfVar [ Dependency ] Executable) -> IO State
goExecutable prefix state (name, CondNode exe deps conf) = do
  pure $ state {
    sCondExecutables = (sCondExecutables state) <> [ (name, CondNode (fixExecutableSrc prefix exe) deps conf ) ]
  }

goLibrary :: State -> GenericPackageDescription -> IO State
goLibrary state pkg = do
  case (condLibrary pkg) of
    Just node -> do
      let
        pkgname :: PackageName
        pkgname = pkgName $ package $ packageDescription pkg
        eModules = exposedModules $ condTreeData node
        newState = state {
            sExposedModules = (sExposedModules state) <> eModules
          , sLibDepends = (sLibDepends state) <> (S.fromList $ targetBuildDepends $ libBuildInfo $ condTreeData node)
          , sNamesToExclude = (sNamesToExclude state) <> [ pkgname ]
          , sLibExtensions = (sLibExtensions state) <> (S.fromList $ defaultExtensions $ libBuildInfo $ condTreeData node)
          , sLibOtherModules = (sLibOtherModules state) <> (S.fromList $ otherModules $ libBuildInfo $ condTreeData node)
          }
      pure newState
    Nothing -> do
      pure state

go :: State -> String -> IO State
go state cabalFile = do
  pkg <- readGenericPackageDescription silent cabalFile
  print cabalFile
  let
    prefix = directory (fromText $ T.pack cabalFile)
    finalPrefix = T.unpack $ fromRight undefined $ toText prefix
  middle <- goLibrary state pkg
  withExecutables <- foldlM (goExecutable finalPrefix) middle (condExecutables pkg)
  final <- foldlM (goTest finalPrefix) withExecutables (condTestSuites pkg)
  pure $ final {
      sExecutables = (sExecutables final) <> (executables $ packageDescription pkg)
    }

goTest :: String -> State -> (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite) -> IO State
goTest prefix state (name, CondNode test a b ) = do
  let
    newTest = test {
      testBuildInfo = (testBuildInfo test) {
        hsSourceDirs = map (prefix <>) (hsSourceDirs $ testBuildInfo test)
      }
    }
  pure $ state {
    sConfTestSuites = (sConfTestSuites state) <> [ (name, CondNode newTest a b) ]
  }

exeFilter :: State -> Executable -> Executable
exeFilter result exe = exe {
  buildInfo = (buildInfo exe) {
    targetBuildDepends = (filter (libFilter result) (targetBuildDepends (buildInfo exe))) <> [ Dependency "everything" anyVersion ]
  }
}

libFilter :: State-> Dependency -> Bool
libFilter result dep = notElem (depPkgName dep) (sNamesToExclude result)

filterCondExecutables :: State -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
filterCondExecutables result = map $ exeFilter2 result

filterCondTests :: State -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)]
filterCondTests result = map $ testFilter2 result

filterExecutables :: State -> [ Executable ] -> [ Executable ]
filterExecutables result = map (exeFilter result)

exeFilter2 :: State -> (UnqualComponentName, CondTree ConfVar [Dependency] Executable) -> (UnqualComponentName, CondTree ConfVar [Dependency] Executable)
exeFilter2 result (name, CondNode exe deps conf) = (name, CondNode (exeFilter result exe) (filter (libFilter result) deps) conf)

testFilter2 :: State -> (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite) -> (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)
testFilter2 result (name, CondNode test a b) = do
  let
    newTest = test {
      testBuildInfo = (testBuildInfo test) {
        targetBuildDepends = (filter (libFilter result) (targetBuildDepends (testBuildInfo test))) <> [ Dependency "everything" anyVersion ]
      }
    }
  (name, CondNode newTest a b)

filteredLibDepends :: State -> [Dependency]
filteredLibDepends result = filter (libFilter result) (S.toList $ sLibDepends result)

pathsFilter :: ModuleName -> Bool
pathsFilter = not . isPrefixOf "Paths_" . toFilePath

main :: IO ()
main = do
  result <- getArgs >>= foldlM go (State [] S.empty [] S.empty S.empty [] [] [])
  let
    mergedLib = emptyLibrary {
        exposedModules = filter pathsFilter $ sExposedModules result
      , libBuildInfo = emptyBuildInfo {
            buildable = True
          , defaultLanguage = Just Haskell2010
          , defaultExtensions = S.toList $ sLibExtensions result
          , otherModules = (filter pathsFilter $ S.toList $ sLibOtherModules result) <> [ "Pos.Infra.Util.SigHandler", "Paths_everything" ]
          , hsSourceDirs = [
              "acid-state-exts/src"
            , "binary/src"
            , "binary/test"
            , "chain/src"
            , "chain/test"
            , "client/src"
            , "core/src"
            , "core/test"
            , "crypto"
            , "crypto/test"
            , "db/src"
            , "db/test"
            , "generator/src"
            , "infra/src"
            , "infra/test"
            , "lib/src"
            , "networking/src"
            , "node-ipc/src"
            , "tools/src"
            , "util/src"
            , "util/test"
            , "utxo/src"
            , "wallet-new/src"
            , "wallet/src"
            , "wallet/test"
            , "x509/src"
            ]
          , targetBuildDepends = (filteredLibDepends result) <> ([ Dependency "unix" anyVersion, Dependency "systemd" anyVersion ])
          }
      }
    libNode :: CondTree ConfVar [Dependency] Library
    libNode = CondNode {
        condTreeComponents = []
      , condTreeData = mergedLib
      , condTreeConstraints = []
      }
    pkgDesc = emptyPackageDescription {
        package = PackageIdentifier {pkgName = mkPackageName "everything", pkgVersion = mkVersion [1,3,0]}
      , licenseRaw = Right MIT
      , specVersionRaw = Right (orLaterVersion (mkVersion [1,10]))
      , buildTypeRaw = Just Simple
      , executables = filterExecutables result $ sExecutables result
      , maintainer = "operations@iohk.io"
      , homepage = "https://github.com/input-output-hk/cardano-sl/#readme"
      }
    genPackage = emptyGenericPackageDescription {
        packageDescription = pkgDesc
      , condLibrary = Just libNode
      , condExecutables = filterCondExecutables result $ sCondExecutables result
      , condTestSuites = filterCondTests result $ sConfTestSuites result
      }
  writeGenericPackageDescription "output" genPackage
