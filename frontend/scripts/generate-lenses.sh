#!/bin/sh

#BACKEND

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

#FRONTEND

set -e

# - - - - - - - - - - -
# Types
# - - - - - - - - - - -

DIR_TYPES=src/Explorer/Types
DIR_TYPES_LENSES=src/Explorer/Lenses

mkdir -p $DIR_TYPES_LENSES

purescript-derive-lenses \
  < $DIR_TYPES/State.purs \
  --moduleName Explorer.Lenses.State \
  --moduleImports "import Pos.Explorer.Socket.Methods (Subscription)" \
  > $DIR_TYPES_LENSES/State.purs

# - - - - - - - - - - -
# I18n
# - - - - - - - - - - -

DIR_I18N=src/Explorer/I18n

purescript-derive-lenses \
  < $DIR_I18N/Types.purs \
  --moduleName Explorer.I18n.Lenses \
  > $DIR_I18N/Lenses.purs

# - - - - - - - - - - -
# Data.Time
# - - - - - - - - - - -

DIR_TIME=src/Data/Time
DIR_TIME_LENSES=$DIR_TIME/Lenses

mkdir -p $DIR_TIME_LENSES

purescript-derive-lenses \
    < $DIR_TIME/NominalDiffTime.purs \
    --moduleName Data.Time.NominalDiffTime.Lenses \
    --moduleImports "import Data.Time.Duration  (Seconds (..))" \
    > $DIR_TIME_LENSES/NominalDiffTime.purs

# - - - - - - - - - - -
# View
# - - - - - - - - - - -

DIR_VIEW=src/Explorer/View
DIR_VIEW_LENSES=$DIR_VIEW/Lenses

mkdir -p $DIR_VIEW_LENSES

purescript-derive-lenses \
    < $DIR_VIEW/Types.purs \
    --moduleName Explorer.View.Lenses \
    --moduleImports "import Data.Time.NominalDiffTime (NominalDiffTime(..))" \
    --moduleImports "import Data.Maybe (Maybe)" \
    --moduleImports "import Data.Tuple (Tuple)" \
    --moduleImports "import Pos.Core.Types (Coin)" \
    --moduleImports "import Pos.Explorer.Web.ClientTypes (CAddress, CTxId)" \
    > $DIR_VIEW_LENSES/View.purs
