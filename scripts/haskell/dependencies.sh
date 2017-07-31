#!/usr/bin/env stack
-- stack runghc --package turtle --package algebraic-graphs-0.0.5 --package parallel

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
import           Algebra.Graph
import           Algebra.Graph.Export.Dot    (Attribute (..), Style (..), export)
import           Control.Monad
import           Control.Parallel.Strategies (parMap, rpar)
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import           Text.Printf
import           Text.Read
import           Turtle                      hiding (export, printf)

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

--------------------------------------------------------------------------------
blacklistedPackages :: [T.Text]
blacklistedPackages = [ "cardano-sl-lwallet"
                      , "cardano-sl-tools"
                      ]

--------------------------------------------------------------------------------
-- Filter `cardano-sl-lwallet` & `cardano-sl-tools` as they cannot be
-- found by `ghc-pkg`, for some reason.
getTotalPackages :: IO [Package]
getTotalPackages = do
    (_, rawList) <- shellStrict "stack list-dependencies --test --bench" mempty
    return $ map mkPackage (filter (not . blacklisted) (T.lines rawList))
    where
      blacklisted x = or $ map (flip T.isInfixOf x) blacklistedPackages

--------------------------------------------------------------------------------
directDependenciesFor :: Package -> IO [Package]
directDependenciesFor (name, ver) = do
    (_, rawOutput) <- shellStrict ("stack exec ghc-pkg field " <> name <> " depends") mempty
    return $ case map T.strip (T.lines rawOutput) of
        ("depends:" : deps) ->
            concatMap (map (mkPackage . normalisePackage) . T.splitOn " ") (takeWhile (/= "depends:") deps)
        _ -> mempty

type DAG    = Graph Package
type DepMap = M.Map Package [Package]

--------------------------------------------------------------------------------
buildDependencyContext :: [Package] -> IO (DepMap, DAG)
buildDependencyContext [] = return (M.empty, Algebra.Graph.empty)
buildDependencyContext pkgs = go pkgs (M.empty, Set.empty)
  where
    go :: [Package] -> (DepMap, Set.Set (Package, Package)) -> IO (DepMap, DAG)
    go [] (depMap, dag) = return (depMap, edges (Set.toList dag))
    go (pkg:xs) (depMap, dag) = do
      directDeps <- directDependenciesFor pkg
      let !newMap = M.insert pkg directDeps $! depMap
      let !newDag = dag <> Set.fromList (map (pkg,) directDeps)
      go xs (newMap, newDag)

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
reverseDependenciesFor :: Package -> [Package] -> DepMap -> DAG -> [Package]
reverseDependenciesFor pkg allDeps directDeps dag = go allDeps mempty
  where
    go [] revDeps     = revDeps
    go (x:xs) revDeps = case reachableFrom x pkg directDeps of
        True  -> go xs (x : revDeps)
        False -> go xs revDeps
        -- For each package x, check the graph to see if there is a path going
        -- from x to `pkg`. If there is, we found a reverse dep.

reachableFrom :: Package -> Package -> DepMap -> Bool
reachableFrom start end directDeps = go (M.findWithDefault mempty start directDeps)
  where
    go :: [Package] -> Bool
    go [] = False
    go (x:xs) = case x == end of
        True  -> True
        False -> any (\newStart -> reachableFrom newStart end directDeps) xs

--------------------------------------------------------------------------------
style :: Style Package String
style = Style
    { graphName               = ""
    , preamble                = ""
    , graphAttributes         = ["label" := "Example", "labelloc" := "top"]
    , defaultVertexAttributes = ["shape" := "circle"]
    , defaultEdgeAttributes   = mempty
    , vertexName              = \(name,_)   -> T.unpack name
    , vertexAttributes        = \_   -> ["color" := "blue"]
    , edgeAttributes          = \_ _ -> ["style" := "dashed"]
    }

--------------------------------------------------------------------------------
dottify :: DAG -> IO ()
dottify dag = writeFile "dep_dot.graphviz" (export style dag)

--------------------------------------------------------------------------------
main :: IO ()
main = do
    allDeps <- getTotalPackages
    putStrLn "Building direct dependency map..."
    (directDepMap, depDag) <- buildDependencyContext allDeps

    let tableHeader         =  printf "%-40s" ("Package" :: String)
                            <> printf "%-20s" ("Direct dependencies"  :: String)
                            <> printf "%-20s" ("Reverse dependencies" :: String)
    let tableEntry pkg deps revDeps =  printf "%-40s" (T.unpack pkg)
                                    <> printf "%-20s" (show deps)
                                    <> printf "%-20s\n" (show (revDeps :: Int))
    putStrLn tableHeader

    let depsMap = M.map length directDepMap

    let sortedDepList = reverse (sortOn snd $ M.toList depsMap)
    let mkTableEntry  (pkg@(pkgName,_), deps) =
            let revDeps = reverseDependenciesFor pkg allDeps directDepMap depDag
            in tableEntry pkgName deps (length revDeps)
    let table         = parMap rpar mkTableEntry sortedDepList

    putStrLn $ mconcat table
    -- Display the total deps
    putStrLn $ tableEntry "Total project deps" (length allDeps + length blacklistedPackages) 0
