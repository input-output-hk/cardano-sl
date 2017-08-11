# So we can run it from a relative path
ROOT_DIR=${1:-.}

DIR_GENERATED_WEB=$ROOT_DIR/src/Generated/Pos/Explorer/Web
DIR_GENERATED_TYPES=$ROOT_DIR/src/Generated/Pos/Core
DIR_TYPES=$ROOT_DIR/src/Explorer/Types
DIR_TYPES_LENSES=$ROOT_DIR/src/Explorer/Lenses
DIR_I18N=$ROOT_DIR/src/Explorer/I18n
DIR_TIME=$ROOT_DIR/src/Data/Time
DIR_TIME_LENSES=$ROOT_DIR/$DIR_TIME/Lenses
DIR_VIEW=$ROOT_DIR/src/Explorer/View
DIR_VIEW_LENSES=$ROOT_DIR/$DIR_VIEW/Lenses

rm -rf $DIR_GENERATED_WEB
rm -rf $DIR_GENERATED_TYPES

rm -rf $DIR_TYPES
rm -rf $DIR_TYPES_LENSES

rm -rf $DIR_I18N

rm -rf $DIR_TIME
rm -rf $DIR_TIME_LENSES

rm -rf $DIR_VIEW
rm -rf $DIR_VIEW_LENSES

rm -rf .psci_modules/ .pulp-cache/ node_modules/ bower_components/ output/
yarn install
./scripts/generate-explorer-lenses.sh
yarn build:prod
echo "Done generating explorer purescript frontend."