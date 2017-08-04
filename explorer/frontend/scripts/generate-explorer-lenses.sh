#!/bin/sh

# So we can run it from a relative path
ROOT_DIR=${1:-.}

# Define and remove all lenses so we can be sure it's a fresh new state.
# Also, if we ever decide to remove a generated Lens folder, we can add it here.
DIR_GENERATED_WEB=$ROOT_DIR/src/Generated/Pos/Explorer/Web/Lenses
DIR_GENERATED_TYPES=$ROOT_DIR/src/Generated/Pos/Core/Lenses
DIR_TYPES_LENSES=$ROOT_DIR/src/Explorer/Lenses
DIR_TIME_LENSES=$ROOT_DIR/$DIR_TIME/Lenses
DIR_VIEW_LENSES=$ROOT_DIR/$DIR_VIEW/Lenses

rm -rf $DIR_GENERATED_WEB
rm -rf $DIR_GENERATED_TYPES
rm -rf $DIR_TYPES_LENSES
rm -rf $DIR_TIME_LENSES
rm -rf $DIR_VIEW_LENSES

#BACKEND

set -e

mkdir -p $DIR_GENERATED_WEB

purescript-derive-lenses \
    < $DIR_GENERATED_WEB/../ClientTypes.purs \
    --moduleName Pos.Explorer.Web.Lenses.ClientTypes \
    --moduleImports "import Data.Maybe" \
    --moduleImports "import Data.Tuple" \
    --moduleImports "import Data.Time.NominalDiffTime (NominalDiffTime(..))" \
    > $DIR_GENERATED_WEB/ClientTypes.purs


mkdir -p $DIR_GENERATED_TYPES

purescript-derive-lenses \
  < $DIR_GENERATED_TYPES/../Types.purs \
  --moduleName Pos.Core.Lenses.Types \
  > $DIR_GENERATED_TYPES/Types.purs

#FRONTEND

set -e

# - - - - - - - - - - -
# Types
# - - - - - - - - - - -

DIR_TYPES=$ROOT_DIR/src/Explorer/Types

mkdir -p $DIR_TYPES_LENSES

purescript-derive-lenses \
  < $DIR_TYPES/State.purs \
  --moduleName Explorer.Lenses.State \
  --moduleImports "import Explorer.Api.Types (SocketSubscription, SocketSubscriptionData)" \
  --moduleImports "import Waypoints (Waypoint)" \
  --moduleImports "import Explorer.Routes (Route)" \
  > $DIR_TYPES_LENSES/State.purs

# - - - - - - - - - - -
# I18n
# - - - - - - - - - - -

DIR_I18N=$ROOT_DIR/src/Explorer/I18n

purescript-derive-lenses \
  < $DIR_I18N/Types.purs \
  --moduleName Explorer.I18n.Lenses \
  > $DIR_I18N/Lenses.purs

# - - - - - - - - - - -
# Data.Time
# - - - - - - - - - - -

DIR_TIME=$ROOT_DIR/src/Data/Time

mkdir -p $DIR_TIME_LENSES

purescript-derive-lenses \
    < $DIR_TIME/NominalDiffTime.purs \
    --moduleName Data.Time.NominalDiffTime.Lenses \
    --moduleImports "import Data.Time.Duration  (Seconds (..))" \
    > $DIR_TIME_LENSES/NominalDiffTime.purs

# - - - - - - - - - - -
# View
# - - - - - - - - - - -

DIR_VIEW=$ROOT_DIR/src/Explorer/View

mkdir -p $DIR_VIEW_LENSES

purescript-derive-lenses \
    < $DIR_VIEW/Types.purs \
    --moduleName Explorer.View.Lenses \
    --moduleImports "import Data.Time.NominalDiffTime (NominalDiffTime(..))" \
    --moduleImports "import Data.Maybe (Maybe)" \
    --moduleImports "import Data.Tuple (Tuple)" \
    --moduleImports "import Pos.Explorer.Web.ClientTypes (CCoin, CAddress, CTxId)" \
    > $DIR_VIEW_LENSES/View.purs
