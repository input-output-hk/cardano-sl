############################################################################
# Windows build script for Buildkite
#
# Requires the devops buildkite-windows AMI
############################################################################

#Set-PSDebug -Trace 1

Write-Output "--- Prepare work directory"

# Avoid long paths on Windows
$env:STACK_ROOT="Z:\s"
$env:STACK_WORK=".w"
$env:WORK_DIR="Z:\w"
# Override the temp directory to avoid sed escaping issues
# See https://github.com/haskell/cabal/issues/5386
$env:TMP="Z:\\tmp"

# Store the original checkout directory
$env:CHECKOUT_PATH=(Get-Item -Path ".\").FullName

# Temp directory needs to exist
New-Item -ItemType Directory -Force -Path $env:TMP

New-Item -ItemType Directory -Force -Path z:\ghc

New-Item -ItemType Directory -Force -Path z:\Downloads
New-Item -ItemType Directory -Force -Path $env:STACK_ROOT

$StackConfig = @"
templates:
  params: null
system-ghc: true
local-programs-path: "z:\\s\\programs"
local-bin-path: "z:\\s\\bin"
"@

$StackConfig | Out-File -FilePath $env:STACK_ROOT\config.yaml -Encoding ASCII

#
if (!([System.IO.File]::Exists("Z:\ghc\ghc-8.4.4.tar.xz"))) {
  echo "Downloading and extracting GHC"
  curl.exe https://s3.eu-central-1.amazonaws.com/ci-static/ghc-8.4.4-x86_64-unknown-mingw32-20181113-b907eb0f9b.tar.xz -o Z:\ghc\ghc-8.4.4.tar.xz -L
  7z x Z:\ghc\ghc-8.4.4.tar.xz -oZ:\ghc
  7z x Z:\ghc\ghc-8.4.4.tar -oZ:\ghc
}

# OpenSSL
#
#$env:USERPROFILE
if (!([System.IO.File]::Exists("Z:\Downloads\Win64OpenSSL-1_0_2q.exe"))) {
  echo "Downloading and installing OpenSSL"
  rd -r -fo z:\OpenSSL-Win64-v102
  curl.exe https://s3.eu-central-1.amazonaws.com/ci-static/Win64OpenSSL-1_0_2q.exe -o Z:\Downloads\Win64OpenSSL-1_0_2q.exe -L
  cmd /c start /wait "Z:\Downloads\Win64OpenSSL-1_0_2q.exe" /silent /verysilent /sp- /suppressmsgboxes /DIR=Z:\OpenSSL-Win64-v102
}
## Install stack
if (!([System.IO.File]::Exists("Z:\Downloads\stack-1.9.1-windows-x86_64.zip"))) {
  echo "Downloading and extracting stack"
  rd -r -fo z:\stack
  curl.exe https://s3.eu-central-1.amazonaws.com/ci-static/stack-1.9.1-windows-x86_64.zip -o z:\Downloads\stack-1.9.1-windows-x86_64.zip -L
  7z -oZ:\stack x Z:\Downloads\stack-1.9.1-windows-x86_64.zip
}

$env:PATH="$env:PATH;Z:\ghc\ghc-8.4.4\bin;Z:\stack;Z:\w"

# Install liblzma/xz
if (!([System.IO.File]::Exists("Z:\Downloads\xz-5.2.3-windows.zip"))) {
  echo "Downloading and extracting xz"
  rd -r -fo z:\xz_extracted
  curl.exe -L https://s3.eu-central-1.amazonaws.com/ci-static/xz-5.2.3-windows.zip -o Z:\Downloads\xz-5.2.3-windows.zip
  7z -oZ:\xz_extracted x Z:\Downloads\xz-5.2.3-windows.zip
}

# Clear out work directory, copy source tree there, then cd into it.
rd -r -fo $env:WORK_DIR
mkdir $env:WORK_DIR
copy-item $env:CHECKOUT_PATH\* $env:WORK_DIR -force -recurse
cd $env:WORK_DIR

# Install pre-built patched version of rocksdb. The git repo provides
# headers and the zip provides rocksdb.dll. This is needed to build
# rocksdb-haskell.
git.exe clone https://github.com/facebook/rocksdb.git --branch v4.13.5
if (!([System.IO.File]::Exists("Z:\Downloads\rocksdb-325427fc709183c8fdf777ad5ea09f8d92bf8585.zip"))) {
  echo "Downloading rocksdb"
  curl.exe -L 'https://s3.eu-central-1.amazonaws.com/ci-static/serokell-rocksdb-haskell-325427fc709183c8fdf777ad5ea09f8d92bf8585.zip' -o Z:\Downloads\rocksdb-325427fc709183c8fdf777ad5ea09f8d92bf8585.zip
}
7z x Z:\Downloads\rocksdb-325427fc709183c8fdf777ad5ea09f8d92bf8585.zip

# CSL-1509: After moving the 'cardano-sl' project itself into a separate folder ('lib/'), the 'cardano-text.exe' executable fails on AppVeyor CI.
# After some investigation, it was discovered that this was because 'rocksdb.dll' has to be located in this folder as well, or else the test executable doesn't work.
copy rocksdb.dll node
copy rocksdb.dll lib
copy rocksdb.dll wallet-new

# Terminate on all errors
$ErrorActionPreference = "Stop"

############################################################################
# Prepare stack and build tools
#

Write-Output "--- Stack setup and install tools"

# Make stack use a pached system-ghc. This will allow us to work around the command
# line argument limit on windows by having GHC copy all libraries into a common folder
# prior to linking; and thus reduces the number library search patch flags passed on the
# command line.
#
# WARNING: if you bump the LTS, make sure you have the right (patched) GHC version in path!
stack.exe config --system-ghc set system-ghc --global true

stack.exe path
stack.exe exec -- ghc-pkg recache
stack.exe --verbosity warn setup --no-reinstall

# Install happy separately: https://github.com/commercialhaskell/stack/issues/3151#issuecomment-310642487. Also install cpphs because it's a build-tool and Stack can't figure out by itself that it should be installed
stack.exe --verbosity warn install happy cpphs -j 2 --no-terminal --local-bin-path $env:SYSTEMROOT\system32 --extra-include-dirs="Z:\OpenSSL-Win64-v102\include" --extra-lib-dirs="Z:\OpenSSL-Win64-v102" --extra-include-dirs="Z:\xz_extracted\include" --extra-lib-dirs="Z:\xz_extracted\bin_x86-64" --extra-include-dirs="$env:WORK_DIR\rocksdb\include" --extra-lib-dirs="$env:WORK_DIR"

############################################################################
# Build and test.
#
# We intentionally don't build auxx here, because this build is for installer.
#

Write-Output "+++ Stack build and test"

stack.exe --dump-logs install cardano-sl cardano-sl-tools cardano-sl-wallet-new -j 3 --no-terminal --local-bin-path $env:WORK_DIR --no-haddock-deps --flag cardano-sl-core:-asserts --flag cardano-sl-tools:for-installer --extra-include-dirs="Z:\OpenSSL-Win64-v102\include" --extra-lib-dirs="Z:\OpenSSL-Win64-v102" --extra-include-dirs="Z:\xz_extracted\include" --extra-lib-dirs="Z:\xz_extracted\bin_x86-64" --extra-include-dirs="$env:WORK_DIR\rocksdb\include" --extra-lib-dirs="$env:WORK_DIR"

############################################################################
# Assemble artifact zip file for use by the Daedalus installers build
#

Write-Output "--- Create zip file"

# Cardano pieces, modulo the frontend
mkdir daedalus
## log config is called `log-config-prod.yaml` just in case, it's the old name
copy log-configs\daedalus.yaml daedalus\log-config-prod.yaml
copy lib\configuration.yaml daedalus\
copy lib\*genesis*.json daedalus\
copy cardano-launcher.exe daedalus\
copy cardano-node.exe daedalus\
copy cardano-x509-certificates.exe daedalus\
cd daedalus
$env:BUILDKITE_BUILD_NUMBER | Out-File -Encoding ASCII build-id
$env:BUILDKITE_COMMIT | Out-File -Encoding ASCII commit-id
$env:BUILDKITE_BUILD_URL | Out-File -Encoding ASCII ci-url
cd ..

$daedaluszip = "$env:BUILDKITE_COMMIT.zip"
7z.exe a $daedaluszip .\daedalus\*
buildkite-agent artifact upload $daedaluszip
