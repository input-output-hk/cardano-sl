{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Common (Package (pkgDependencies, pkgExtensions, pkgMainIs, pkgName, pkgPackages, pkgSrc),
                     cabalFilesToPackages)
import           Data.Fix (Fix (Fix))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           Data.Text (unpack)
import           Nix.Expr.Shorthands (mkFunction, mkList, mkNonRecSet, mkPath,
                     mkStr, mkSym, ($=))
import           Nix.Expr.Types (Binding (NamedVar), NExpr, NExprF (NSelect),
                     NKeyName (StaticKey), Params (Param), nullPos)
import           Nix.Pretty (prettyNix)
import           System.Environment (getArgs)

packageToExpr :: Package -> (Binding NExpr)
packageToExpr pkg = NamedVar (NonEmpty.fromList [ StaticKey $ pkgName pkg ]) set nullPos
  where
    set :: NExpr
    set = mkNonRecSet ([
        "src" $= (mkPath False $ unpack $ pkgSrc pkg)
      , "dependencies" $= (mkList $ Set.toList $ Set.map mkStr (pkgDependencies pkg))
      , "extensions" $= (mkList $ Set.toList $ Set.map mkStr (pkgExtensions pkg))
      , "packages" $= (mkList $ Set.toList $ Set.map toAttrpath (pkgPackages pkg))
      ] <> (makeMain $ pkgMainIs pkg) )
    makeMain (Just mainModule) = [
        "main" $= ( mkStr mainModule)
      ]
    makeMain Nothing = []
    toAttrpath name = Fix (NSelect (mkSym "self") (NonEmpty.fromList [ StaticKey name ]) Nothing)

main :: IO ()
main = do
  res2 <- getArgs >>= cabalFilesToPackages
  let
    set2 = mkNonRecSet $ map packageToExpr res2
    func = mkFunction (Param "self") set2
  writeFile "output" $ (show . prettyNix) func
