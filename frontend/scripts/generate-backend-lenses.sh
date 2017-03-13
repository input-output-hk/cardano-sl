#!/bin/sh

set -e

DIR_GENERATED_WEB=src/Generated/Pos/Explorer/Web

mkdir -p $DIR_GENERATED_WEB/Lenses

purescript-derive-lenses \
    < $DIR_GENERATED_WEB/ClientTypes.purs \
    --moduleName Pos.Explorer.Web.Lenses.ClientTypes \
    --moduleImports "import Data.Maybe" \
    --moduleImports "import Data.Tuple" \
    --moduleImports "import Data.Time.NominalDiffTime (NominalDiffTime(..))" \
    --moduleImports "import Pos.Core.Types (Coin)" \
    > $DIR_GENERATED_WEB/Lenses/ClientTypes.purs


DIR_GENERATED_TYPES=src/Generated/Pos/Core

mkdir -p $DIR_GENERATED_TYPES/Lenses

purescript-derive-lenses \
  < $DIR_GENERATED_TYPES/Types.purs \
  --moduleName Pos.Core.Lenses.Types \
  > $DIR_GENERATED_TYPES/Lenses/Types.purs
