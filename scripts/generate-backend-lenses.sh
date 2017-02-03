#!/bin/bash

set -e

DIR_GENERATED_WEB=src/Generated/Pos/Explorer/Web

mkdir -p $DIR_GENERATED_WEB/Lenses

purescript-derive-lenses \
    < $DIR_GENERATED_WEB/ClientTypes.purs \
    --moduleName Pos.Explorer.Web.Lenses.ClientTypes \
    --moduleImports "import Data.Maybe" \
    --moduleImports "import Data.Types" \
    --moduleImports "import Pos.Types.Core" \
    > $DIR_GENERATED_WEB/Lenses/ClientTypes.purs


DIR_GENERATED_TYPES=src/Generated/Pos/Types

mkdir -p $DIR_GENERATED_TYPES/Lenses

purescript-derive-lenses \
  < $DIR_GENERATED_TYPES/Core.purs \
  --moduleName Pos.Types.Lenses.Core \
  > $DIR_GENERATED_TYPES/Lenses/Core.purs
