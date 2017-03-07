#!/bin/sh

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
