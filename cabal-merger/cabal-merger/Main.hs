{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Common (State (State, sCondExecutables, sCondTestSuites, sExecutables, sExposedModules, sFlags, sLibConditions, sLibDepends, sLibExtensions, sLibOtherModules, sNamesToExclude),
                     filteredLibDepends, go, libFilter)
import           Data.Foldable (foldlM)
import           Data.List (isPrefixOf)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Distribution.License (License (MIT))
import           Distribution.ModuleName (ModuleName, toFilePath)
import           Distribution.Package (Dependency (Dependency),
                     PackageIdentifier (PackageIdentifier, pkgName, pkgVersion),
                     mkPackageName)
import           Distribution.PackageDescription (BuildInfo (buildable, defaultExtensions, defaultLanguage, hsSourceDirs, otherModules, targetBuildDepends),
                     BuildType (Simple),
                     CondTree (CondNode, condTreeComponents, condTreeConstraints, condTreeData),
                     ConfVar, Executable (buildInfo),
                     Library (exposedModules, libBuildInfo),
                     PackageDescription (buildTypeRaw, homepage, licenseRaw, maintainer, package, specVersionRaw),
                     emptyBuildInfo, emptyLibrary, emptyPackageDescription,
                     executables)
import           Distribution.PackageDescription.PrettyPrint
                     (writeGenericPackageDescription)
import           Distribution.Types.GenericPackageDescription (GenericPackageDescription (condExecutables, condLibrary, condTestSuites, genPackageFlags, packageDescription),
                     emptyGenericPackageDescription)
import           Distribution.Types.TestSuite (TestSuite (testBuildInfo))
import           Distribution.Types.UnqualComponentName (UnqualComponentName)
import           Distribution.Version (anyVersion, mkVersion, orLaterVersion)
import           Language.Haskell.Extension (Language (Haskell2010))
import           System.Environment (getArgs)

exeFilter :: State -> Executable -> Executable
exeFilter result exe = exe {
  buildInfo = (buildInfo exe) {
    targetBuildDepends = (filter (libFilter True (sNamesToExclude result)) (targetBuildDepends (buildInfo exe))) <> [ Dependency "everything" anyVersion ]
  , otherModules = (otherModules (buildInfo exe)) <> [ "Paths_everything" ]
  }
}

filterCondExecutables :: State -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
filterCondExecutables result = map $ exeFilter2 result

filterCondTests :: State -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)]
filterCondTests result = map $ testFilter2 result

filterExecutables :: State -> [ Executable ] -> [ Executable ]
filterExecutables result = map (exeFilter result)

exeFilter2 :: State -> (UnqualComponentName, CondTree ConfVar [Dependency] Executable) -> (UnqualComponentName, CondTree ConfVar [Dependency] Executable)
exeFilter2 result (name, CondNode exe deps conf) = (name, CondNode (exeFilter result exe) (filter (libFilter True (sNamesToExclude result)) deps) conf)

testFilter2 :: State -> (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite) -> (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)
testFilter2 result (name, CondNode test a b) = do
  let
    newTest = test {
      testBuildInfo = (testBuildInfo test) {
        targetBuildDepends = (filter (libFilter True (sNamesToExclude result)) (targetBuildDepends (testBuildInfo test))) <> [ Dependency "everything" anyVersion ]
      }
    }
  (name, CondNode newTest a b)

pathsFilter :: ModuleName -> Bool
pathsFilter = not . isPrefixOf "Paths_" . toFilePath

main :: IO ()
main = do
  result <- getArgs >>= foldlM go (State S.empty S.empty [] S.empty S.empty [] [] [] [] S.empty)
  let
    mergedLib = emptyLibrary {
        exposedModules = filter pathsFilter $ S.toList $ sExposedModules result
      , libBuildInfo = emptyBuildInfo {
            buildable = True
          , defaultLanguage = Just Haskell2010
          , defaultExtensions = S.toList $ sLibExtensions result
          , otherModules = (filter pathsFilter $ S.toList $ sLibOtherModules result) <> [ "Paths_everything" ]
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
            , "faucet/src"
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
          , targetBuildDepends = filteredLibDepends True (sNamesToExclude result) (S.toList $ sLibDepends result)
          }
      }
    libNode :: CondTree ConfVar [Dependency] Library
    libNode = CondNode {
        condTreeComponents = sLibConditions result
      , condTreeData = mergedLib
      , condTreeConstraints = []
      }
    pkgDesc = emptyPackageDescription {
        package = PackageIdentifier {
            Distribution.Package.pkgName = mkPackageName "everything"
          , pkgVersion = mkVersion [1,3,0]
          }
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
      , condTestSuites = filterCondTests result $ sCondTestSuites result
      , genPackageFlags = S.toList $ sFlags result
      }
  writeGenericPackageDescription "output" genPackage
