{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Nix.Expr.Types (Binding(NamedVar), NExprF(NSet, NLiteralPath, NList, NStr, NSelect, NSym, NAbs), VarName, NKeyName(StaticKey), Params(Param))
import Nix.Pretty (prettyNix)
import Data.Fix (Fix(Fix))
import Text.Megaparsec.Pos (mkPos, SourcePos(SourcePos))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (unpack)
import Data.String (fromString)
import qualified Data.Set as Set
import Common (Package(pkgSrc, pkgDependencies, pkgExtensions, pkgMainIs, pkgPackages, pkgName), cabalFilesToPackages)
import           System.Environment (getArgs)

packageToExpr :: Package -> (Binding (Fix NExprF))
packageToExpr pkg = NamedVar (NonEmpty.fromList [ StaticKey $ pkgName pkg ]) (Fix set) pos
  where
    set :: NExprF (Fix NExprF)
    set = NSet ([
        mkBinding "src" (NLiteralPath $ pkgSrc pkg)
      , mkBinding "dependencies" (NList $ Set.toList $ Set.map (Fix . NStr . fromString) (pkgDependencies pkg))
      , mkBinding "extensions" (NList $ Set.toList $ Set.map (Fix . NStr . fromString) (pkgExtensions pkg))
      , mkBinding "packages" (NList $ Set.toList $ Set.map toAttrpath (pkgPackages pkg))
      ] <> (makeMain $ pkgMainIs pkg) )
    makeMain (Just mainModule) = [
        mkBinding "main" ( (NStr . fromString . unpack) mainModule)
      ]
    makeMain Nothing = []
    toAttrpath name = Fix (NSelect (Fix $ NSym "self") (NonEmpty.fromList [ StaticKey name ]) Nothing)
    pos = (SourcePos "file" (mkPos 1) (mkPos 1))
    mkBinding :: VarName -> f (Fix f) -> Binding (Fix f)
    mkBinding key value = NamedVar (NonEmpty.fromList [ StaticKey key ]) (Fix value) pos

main :: IO ()
main = do
  res2 <- getArgs >>= cabalFilesToPackages
  let
    set2 = NSet $ map packageToExpr res2
    func = NAbs (Param "self") (Fix set2)
  writeFile "output" $ (show . prettyNix . Fix) func
