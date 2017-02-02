#!/bin/bash

set -e

DIR_TYPES=src/Explorer/Types
DIR_LENSES=./src/Explorer/Lenses

mkdir -p $DIR_LENSES

purescript-derive-lenses \
  < $DIR_TYPES/State.purs \
  --moduleName Explorer.Lenses.State \
  > $DIR_LENSES/State.purs

DIR_I18N=src/Explorer/I18n

purescript-derive-lenses \
  < $DIR_I18N/Types.purs \
  --moduleName Explorer.I18n.Lenses \
  > $DIR_I18N/Lenses.purs
