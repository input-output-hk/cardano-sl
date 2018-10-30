Set-PSDebug -Trace 1
$env:STACK_ROOT="d:\s"
$env:STACK_WORK= ".w"
$env:WORK_DIR="d:\w"

#  Setup stuff (commented out)
#mkdir d:\ghc
#
#Invoke-WebRequest "https://s3.eu-central-1.amazonaws.com/ci-static/ghc-8.2.2-x86_64-unknown-mingw32.tar.xz" -OutFile "D:\ghc\ghc.tar.xz" -UserAgent "Curl"
#
#7z x D:\ghc\ghc.tar.xz -oD:\ghc
#
#7z x D:\ghc\ghc.tar -oD:\ghc

# OpenSSL
#
#$env:USERPROFILE
#(New-Object Net.WebClient).DownloadFile('https://slproweb.com/download/Win64OpenSSL-1_0_2p.exe', "D:\Downloads\Win64OpenSSL.exe")
#cmd /c start /wait "D:\Downloads\Win64OpenSSL.exe" /silent /verysilent /sp- /suppressmsgboxes /DIR=D:\OpenSSL-Win64-v102
## Install stack
#Start-FileDownload http://www.stackage.org/stack/windows-x86_64 -FileName D:\Downloads\stack.zip
#curl.exe http://www.stackage.org/stack/windows-x86_64 -o d:\Downloads\stack.zip -L
#7z -oD:\stack x D:\Downloads\stack.zip

$env:PATH="$env:PATH;D:\ghc\ghc-8.2.2\bin;d:\stack"

# Install liblzma/xz
#curl -L https://tukaani.org/xz/xz-5.2.3-windows.zip -o xz-5.2.3-windows.zip
#7z -oD:\xz_extracted x xz-5.2.3-windows.zip


# Not needed????
rd -r -fo d:\w
cd d:\w
git clean -fdx
git clone https://github.com/facebook/rocksdb.git --branch v4.13.5
#curl.exe -L 'https://s3.eu-central-1.amazonaws.com/ci-static/serokell-rocksdb-haskell-325427fc709183c8fdf777ad5ea09f8d92bf8585.zip' -o D:\Downloads\rocksdb.zip
7z x D:\Downloads\rocksdb.zip
# RocksDB needs installed into cardano directories

# CSL-1509: After moving the 'cardano-sl' project itself into a separate folder ('lib/'), the 'cardano-text.exe' executable fails on AppVeyor CI.
# After some investigation, it was discovered that this was because 'rocksdb.dll' has to be located in this folder as well, or else the test executable doesn't work.
copy rocksdb.dll node
copy rocksdb.dll lib
copy rocksdb.dll wallet
copy rocksdb.dll wallet-new
# Start doing stuff

stack.exe config --system-ghc set system-ghc --global true
stack.exe exec -- ghc-pkg recache
stack.exe --verbosity warn setup --no-reinstall > nul
# Install happy separately: https://github.com/commercialhaskell/stack/issues/3151#issuecomment-310642487. Also install cpphs because it's a build-tool and Stack can't figure out by itself that it should be installed
stack.exe --verbosity warn install happy cpphs -j 2 --no-terminal --local-bin-path $env:SYSTEMROOT\system32 --extra-include-dirs="D:\OpenSSL-Win64-v102\include" --extra-lib-dirs="D:\OpenSSL-Win64-v102" --extra-include-dirs="D:\xz_extracted\include" --extra-lib-dirs="D:\xz_extracted\bin_x86-64" --extra-include-dirs="$env:WORK_DIR\rocksdb\include" --extra-lib-dirs="$env:WORK_DIR"
# TODO: CSL-1133. To be reenabled.
 stack.exe test --coverage
 stack.exe hpc report cardano-sl cardano-sl-txp cardano-sl-core cardano-sl-db cardano-sl-update cardano-sl-infra cardano-sl-lrc cardano-sl-ssc
#try transient failures due to https://github.com/haskell/cabal/issues/4005
#We intentionally don't build auxx here, because this build is for installer.
stack.exe --dump-logs install cardano-sl cardano-sl-tools cardano-sl-wallet cardano-sl-wallet-new -j 3 --no-terminal --local-bin-path $env:WORK_DIR --no-haddock-deps --flag cardano-sl-core:-asserts --flag cardano-sl-tools:for-installer --flag cardano-sl-wallet:for-installer --extra-include-dirs="D:\OpenSSL-Win64-v102\include" --extra-lib-dirs="D:\OpenSSL-Win64-v102" --extra-include-dirs="D:\xz_extracted\include" --extra-lib-dirs="D:\xz_extracted\bin_x86-64" --extra-include-dirs="%WORK_DIR%\rocksdb\include" --extra-lib-dirs="$env:WORK_DIR"
#Cardano pieces, modulo the frontend
mkdir daedalus
## log config is called `log-config-prod.yaml` just in case, it's the old name
copy log-configs\daedalus.yaml daedalus\log-config-prod.yaml
copy lib\configuration.yaml daedalus\
copy lib\*genesis*.json daedalus\
copy cardano-launcher.exe daedalus\
copy cardano-node.exe daedalus\
copy cardano-x509-certificates.exe daedalus\
cd daedalus
#Echo %APPVEYOR_BUILD_VERSION% > build-id
#Echo %APPVEYOR_REPO_COMMIT% > commit-id
