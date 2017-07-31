#!/usr/bin/env stack
-- stack runghc --package turtle --package algebraic-graphs

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
import           Algebra.Graph
import           Control.Monad
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.Set              as Set
import qualified Data.Text             as T
import           Text.Printf
import           Text.Read
import           Turtle                hiding (printf)

type PackageName = T.Text
data Version = V [Int] deriving (Eq, Ord)
type Package = (PackageName, Version)

readVersionM :: Monad m => (String -> m Int) -> T.Text -> m Version
readVersionM f = fmap V . sequence . map (f . T.unpack) . T.splitOn "."

readVersionMaybe :: T.Text -> Maybe Version
readVersionMaybe = readVersionM readMaybe

readVersion :: T.Text -> Version
readVersion = runIdentity . readVersionM (liftM read . pure)

mkPackage :: T.Text -> Package
mkPackage t = case T.splitOn " " (T.strip t) of
    [name, ver] -> (name, readVersion ver)
    _           -> case T.breakOnEnd "-" (T.strip t) of
        ("", _)     -> error $ "mkPackage: " <> show t
        (name, ver) -> (T.init name, readVersion ver)

blacklistedPackages :: [T.Text]
blacklistedPackages = [ "cardano-sl-lwallet"
                      , "cardano-sl-tools"
                      , "Cabal"
                      , "base"
                      , "Glob"]

-- Filter `cardano-sl-lwallet` & `cardano-sl-tools` as they cannot be
-- found by `ghc-pkg`, for some reason.
getTotalPackages :: IO [Package]
getTotalPackages = do
    (_, rawList) <- shellStrict "stack list-dependencies --test --bench" mempty
    return $ map mkPackage (filter (not . blacklisted) (T.lines rawList))
    where
      blacklisted x = or $ map (flip T.isInfixOf x) blacklistedPackages

directDependenciesFor :: Package -> IO [Package]
directDependenciesFor (name, ver) = do
    (_, rawOutput) <- shellStrict ("stack exec ghc-pkg field " <> name <> " depends") mempty
    return $ case map T.strip (T.lines rawOutput) of
        ("depends:" : deps) ->
            concatMap (map (mkPackage . normalisePackage) . T.splitOn " ") (takeWhile (/= "depends:") deps)
        _ -> mempty

type DAG = Graph Package

buildDependencyContext :: [Package] -> IO (M.Map PackageName [Package], DAG)
buildDependencyContext [] = return (M.empty, Algebra.Graph.empty)
buildDependencyContext pkgs = go pkgs (M.empty, Set.empty)
  where
    go :: [Package] -> (M.Map PackageName [Package], Set.Set (Package, Package)) -> IO (M.Map PackageName [Package], DAG)
    go [] (depMap, dag) = return (depMap, edges (Set.toList dag))
    go (pkg@(pkgName,_):xs) (depMap, dag) = do
      directDeps <- directDependenciesFor pkg
      let !newMap = M.insert pkgName directDeps $! depMap
      let !newDag = dag <> Set.fromList (map (pkg,) directDeps)
      go xs (newMap, newDag)

-- | >>> normalisePackage "conduit-1.2.10-GgLn1U1QYcf9wsQecuZ1A4"
-- "conduit-1.2.10"
-- >>> normalisePackage "conduit-1.2.10"
-- "conduit-1.2.10"
normalisePackage :: T.Text -> T.Text
normalisePackage txt = case T.breakOnEnd "-" txt of
    (x, xs) -> case readVersionMaybe xs of
        Just _  -> txt
        Nothing -> if x == "" then error ("normalisePackage: " <> show txt) else T.init x

main :: IO ()
main = do
    allDeps <- getTotalPackages
    putStrLn "Building direct dependency map..."
    (directDepMap, depDag) <- buildDependencyContext allDeps

    let tableHeader         = printf "%-40s" ("Package" :: String) <> printf "%-10s" ("Direct dependencies" :: String)
    let tableEntry pkg deps = printf "%-40s" (T.unpack pkg) <> printf "%-10s" (show deps)
    putStrLn tableHeader

    let depsMap = M.map length directDepMap

    forM_ (sortOn snd $ M.toList depsMap) $ \(pkgName, deps) -> do
        putStrLn (tableEntry pkgName deps)
    -- Display the total deps
    putStrLn $ tableEntry "Total project deps" (length allDeps + length blacklistedPackages)
