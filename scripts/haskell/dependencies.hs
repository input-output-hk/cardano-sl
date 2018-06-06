#!/usr/bin/env stack
-- stack runghc --package shelly --package async

{-

It's warmly recommended to compile this script as a binary, in order to exploit multicore
parallelism, e.g.:

stack exec ghc -- --make -O2 -threaded scripts/haskell/dependencies.hs
./dependencies +RTS -N

-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Algebra.Graph
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map.Strict          as M
import           Data.Monoid
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import           Shelly
import           System.IO
import           Text.Printf
import           Text.Read

--------------------------------------------------------------------------------
type PackageName = T.Text

data Version = V [Int] deriving (Eq, Ord)

type Package = (PackageName, Version)

type DAG       = Graph Package

type DepMap    = M.Map Package [Package]

type RevDepMap = M.Map Package [Package]

--------------------------------------------------------------------------------
readVersionM :: Monad m => (String -> m Int) -> T.Text -> m Version
readVersionM f = fmap V . sequence . map (f . T.unpack) . T.splitOn "."

--------------------------------------------------------------------------------
readVersionMaybe :: T.Text -> Maybe Version
readVersionMaybe = readVersionM readMaybe

--------------------------------------------------------------------------------
readVersion :: T.Text -> Version
readVersion = runIdentity . readVersionM (liftM read . pure)

--------------------------------------------------------------------------------
mkPackage :: T.Text -> Package
mkPackage t = case T.splitOn " " (T.strip t) of
    [name, ver] -> (name, readVersion ver)
    _           -> case T.breakOnEnd "-" (T.strip t) of
        ("", _)     -> error $ "mkPackage: " <> show t
        (name, ver) -> (T.init name, readVersion ver)

--------------------------------------------------------------------------------
blacklistedPackages :: [T.Text]
blacklistedPackages = [ "cardano-sl-auxx"
                      , "cardano-sl-tools"
                      ]

--------------------------------------------------------------------------------
-- Filter `cardano-sl-auxx` & `cardano-sl-tools` as they cannot be
-- found by `ghc-pkg`, for some reason.
getTotalPackages :: IO [Package]
getTotalPackages = do
    rawList <- shelly $ silently $ run "stack" ["list-dependencies", "--test", "--bench"]
    return $ map mkPackage (filter (not . blacklisted) (T.lines rawList))
    where
      blacklisted x = or $ map (flip T.isInfixOf x) blacklistedPackages

--------------------------------------------------------------------------------
directDependenciesFor :: Package -> IO [Package]
directDependenciesFor (name, ver) = do
    rawOutput <- shelly $ silently $ run "stack" ["exec", "ghc-pkg", "field", name, "depends"]
    return $ case map T.strip (T.lines rawOutput) of
        ("depends:" : deps) ->
            concatMap (map (mkPackage . normalisePackage) . T.splitOn " ") (takeWhile (/= "depends:") deps)
        _ -> mempty

--------------------------------------------------------------------------------
buildPackageMap :: forall m. Monad m => (Package -> m [Package]) -> [Package] -> m DepMap
buildPackageMap _ [] = return M.empty
buildPackageMap f pkgs = go pkgs M.empty
  where
    go :: [Package] -> DepMap -> m DepMap
    go [] depMap = return depMap
    go (pkg:xs) depMap = do
      directDeps <- f pkg
      let !newMap = M.insert pkg directDeps $! depMap
      go xs newMap

--------------------------------------------------------------------------------
buildDependencyMap :: [Package] -> IO DepMap
buildDependencyMap allDeps = do
    mapAsList <- mapConcurrently (\pkg -> (pkg,) <$> directDependenciesFor pkg) allDeps
    return $ M.fromList mapAsList

--------------------------------------------------------------------------------
buildReverseDependencyMap :: [Package] -> DepMap -> RevDepMap
buildReverseDependencyMap allDeps depMap =
    runIdentity $ buildPackageMap (Identity . reverseDependenciesFor allDeps depMap) allDeps

--------------------------------------------------------------------------------
buildUniqueDependencyMap :: [Package] -> DepMap -> RevDepMap -> DepMap
buildUniqueDependencyMap allDeps depMap revMap =
    runIdentity $ buildPackageMap (Identity . uniqueDependenciesFor depMap revMap) allDeps

--------------------------------------------------------------------------------
buildDependencyDAG :: [Package] -> DepMap -> IO DAG
buildDependencyDAG allPkgs depMap = go allPkgs Set.empty
  where
    go :: [Package] -> Set.Set (Package, Package) -> IO DAG
    go [] dagEdges = return . edges . Set.toList $ dagEdges
    go (pkg:xs) dagEdges = do
      let directDeps   = M.findWithDefault mempty pkg depMap
      let !newDag = dagEdges <> Set.fromList (map (pkg,) directDeps)
      go xs newDag

--------------------------------------------------------------------------------
-- | >>> normalisePackage "conduit-1.2.10-GgLn1U1QYcf9wsQecuZ1A4"
-- "conduit-1.2.10"
-- >>> normalisePackage "conduit-1.2.10"
-- "conduit-1.2.10"
normalisePackage :: T.Text -> T.Text
normalisePackage "rts" = "rts-0.0.0.0"
normalisePackage txt = case T.breakOnEnd "-" txt of
    (x, xs) -> case readVersionMaybe xs of
        Just _  -> txt
        Nothing -> if x == "" then error ("normalisePackage: " <> show txt) else T.init x


--------------------------------------------------------------------------------
unavoidableDeps :: Package -> Package -> Bool
unavoidableDeps myself x = and [
      x /= myself
    , not ("cardano" `T.isInfixOf` (fst x))
    ]

--------------------------------------------------------------------------------
-- | Filter "unavoilable" dependencies like the ones of the cardano family.
reverseDependenciesFor :: [Package] -> DepMap -> Package -> [Package]
reverseDependenciesFor allDeps directDeps pkg = go (filter (unavoidableDeps pkg) allDeps) mempty
  where
    go [] !revDeps     = revDeps
    go (x:xs) !revDeps = case reachableFrom x of
        True  -> go xs (x : revDeps)
        False -> go xs revDeps
        -- For each package x, check the graph to see if there is a path going
        -- from x to `pkg`. If there is, we found a reverse dep.
    reachableFrom :: Package -> Bool
    reachableFrom directDep =
        let depsForThis = M.findWithDefault mempty directDep directDeps
        in case pkg `elem` depsForThis of
            True  -> True
            False -> go depsForThis
      where
        go :: [Package] -> Bool
        go [] = False
        go xs = any reachableFrom xs

--------------------------------------------------------------------------------
-- | Compute the "unique direct dependencies", which are the dependencies that
-- only this package introduces into the project.
-- In other terms, we need to count for each DIRECT dependency, the number of
-- REVERSE dependencies. If it's one, and it's the package in question, it
-- means that removing that dependency would also remove the associated package.
uniqueDependenciesFor :: DepMap -> RevDepMap -> Package -> [Package]
uniqueDependenciesFor directDeps revDeps pkg = go (M.findWithDefault mempty pkg directDeps) []
    where
      go [] !deps     = deps
      go (d:ds) !deps = case M.findWithDefault mempty d revDeps of
          [x] | x == pkg -> go ds (d : deps)
          _   -> go ds deps

--------------------------------------------------------------------------------
main :: IO ()
main = do
    hSetBuffering System.IO.stdout NoBuffering
    allDeps <- getTotalPackages
    putStr "Building direct dependency map..."
    directDepMap  <- buildDependencyMap allDeps
    putStrLn "ok."
    let revDepMap    = buildReverseDependencyMap allDeps directDepMap
    let uniqueDepMap = buildUniqueDependencyMap allDeps directDepMap revDepMap

    let tableHeader         =  printf "%-40s" ("Package" :: String)
                            <> printf "%-20s" ("Direct deps"  :: String)
                            <> printf "%-20s" ("Unique deps"  :: String)
                            <> printf "%-70s" ("Reverse deps (excluding cardano-*)"  :: String)
    let tableEntry pkg (totalDeps, uniqueDeps) revDeps =
               printf "%-40s" (T.unpack pkg)
            <> printf "%-20s" (show totalDeps)
            <> printf "%-20s" (show uniqueDeps)
            <> printf "%-70s\n" (T.unpack $ showRevDeps revDeps)
    putStrLn tableHeader

    let depsMap = M.map length directDepMap
    let sortedDepList = reverse (sortOn snd $ M.toList depsMap)

    let mkTableEntry  (pkg@(pkgName,_), deps) =
            let revDeps    = M.findWithDefault mempty pkg revDepMap
                uniqueDeps = M.findWithDefault mempty pkg uniqueDepMap
            in tableEntry pkgName (deps, length uniqueDeps) revDeps

    forM_ sortedDepList (putStr . mkTableEntry)

    -- Display the total deps
    putStrLn $ "Total project deps: " <> (show $ length allDeps + length blacklistedPackages)

showRevDeps :: [Package] -> T.Text
showRevDeps []  = T.pack $ printf "%-4d%s" (0 :: Int) ("(possibly cardano depends on it)" :: String)
showRevDeps [(pkgName,_)] = T.pack $ printf "%-4d%s" (1 :: Int) ("(" <> T.unpack pkgName <> ")")
showRevDeps xs
  | length xs <= 5 = T.pack $ printf "%-4d%s" (length xs) (T.unpack $ "(" <> T.intercalate "," (map fst xs) <> ")")
  | otherwise      = T.pack $ printf "%-4d%s" (length xs) (T.unpack $ "(" <> T.intercalate "," (map fst (take 5 xs)) <> ",...)")
