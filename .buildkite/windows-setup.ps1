############################################################################
# Setup for cardano-sl builds on a new Windows machine
#
# 1. Before doing this, you need Chocolatey installed.
#    Best to put it in D:\chocolatey
#
# 2. Set the default tools install directory for choco:
#
#    [System.Environment]::SetEnvironmentVariable('ChocolateyToolsLocation', 'd:\tools')
#
# 3. choco install 7z.install
#    choco install curl
#    choco install vcredist140
#    choco install vcredist-all
#
# 4. Create/edit D:\s\config.yaml:
#
#    system-ghc: true
#    local-programs-path: "d:\\s\\programs"
#    local-bin-path: "d:\\s\\bin"
#
# 5. More details for setting up Buildkite agent in
#    https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1113
#
############################################################################

Set-PSDebug -Trace 1


############################################################################
# Patched GHC 8.4.4 (MAX_PATH fix)
mkdir d:\ghc

Invoke-WebRequest "https://s3.eu-central-1.amazonaws.com/ci-static/ghc-8.4.4-x86_64-unknown-mingw32-20181113-b907eb0f9b.tar.xz" -OutFile "D:\ghc\ghc.tar.xz" -UserAgent "Curl"
7z x D:\ghc\ghc.tar.xz -oD:\ghc
7z x D:\ghc\ghc.tar -oD:\ghc


############################################################################
# OpenSSL

$env:USERPROFILE
(New-Object Net.WebClient).DownloadFile('https://slproweb.com/download/Win64OpenSSL-1_0_2p.exe', "D:\Downloads\Win64OpenSSL.exe")
cmd /c start /wait "D:\Downloads\Win64OpenSSL.exe" /silent /verysilent /sp- /suppressmsgboxes /DIR=D:\OpenSSL-Win64-v102
# Install stack
Start-FileDownload http://www.stackage.org/stack/windows-x86_64 -FileName D:\Downloads\stack.zip
curl.exe http://www.stackage.org/stack/windows-x86_64 -o d:\Downloads\stack.zip -L
7z -oD:\stack x D:\Downloads\stack.zip

# Install liblzma/xz
curl -L https://tukaani.org/xz/xz-5.2.3-windows.zip -o xz-5.2.3-windows.zip
7z -oD:\xz_extracted x xz-5.2.3-windows.zip
