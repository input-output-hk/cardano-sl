cd C:\pos-haskell-prototype\daedalus
npm install
npm run build:prod
npm run build:dev
cd C:\daedalus
npm install
npm install C:\pos-haskell-prototype\daedalus
xcopy C:\pos-haskell-prototype\daedalus\output node_modules\daedalus-client-api\output /E
node_modules\.bin\electron-packager . --icon 64x64 --overwrite
cd C:\pos-haskell-prototype\installers


TODO: http://stackoverflow.com/a/33319828/133235

SET CSL_SYSTEM_TAG=win64
stack build --flag cardano-sl:with-web --flag cardano-sl:with-wallet --flag cardano-sl:-dev-mode --flag cardano-sl:-asserts --extra-include-dirs="C:\OpenSSL-Win64\include" --extra-lib-dirs="C:\OpenSSL-Win64" --extra-lib-dirs="C:\rocksdb\build\Release" --extra-include-dirs="C:\rocksdb\include" --copy-bins --local-bin-path installers


stack build --test --extra-lib-dirs="C:\rocksdb\build\Release" --extra-include-dirs="C:\rocksdb\include"