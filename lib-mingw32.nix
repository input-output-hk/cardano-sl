# mingw32 related functions for cross compiling haskell code to windows
{ pkgs, self }:
with pkgs.haskell.lib;
let buildHaskellPackages = pkgs.buildPackages.haskellPackages; in rec
{
    # Logic to run TH via an external interpreter (64bit windows via wine64)
  doTemplateHaskellMingw32 = pkg: let
    buildTools = [ buildHaskellPackages.iserv-proxy pkgs.buildPackages.winePackages.minimal ];
    buildFlags = map (opt: "--ghc-option=" + opt) [
      "-fexternal-interpreter"
      "-pgmi" "${buildHaskellPackages.iserv-proxy}/bin/iserv-proxy"
      "-opti" "127.0.0.1" "-opti" "$PORT"
      # TODO: this should be automatically injected based on the extraLibrary.
      "-L${pkgs.windows.mingw_w64_pthreads}/lib"
    ];
    preBuild = ''
      # unset the configureFlags.
      # configure should have run already
      # without restting it, wine might fail
      # due to a too large environment.
      unset configureFlags
      PORT=$((5000 + $RANDOM % 5000))
      echo "---> Starting remote-iserv on port $PORT"
      WINEPREFIX=$TMP wine64 ${self.remote-iserv}/bin/remote-iserv.exe tmp $PORT &
      echo "---| remote-iserv should have started on $PORT"
      RISERV_PID=$!
    '';
    postBuild = ''
      echo "---> killing remote-iserv..."
      kill $RISERV_PID
    ''; in
    appendBuildFlags' buildFlags
     (addBuildDepends' [ self.remote-iserv ]
      (addExtraLibrary' pkgs.windows.mingw_w64_pthreads
       (addBuildTools' buildTools
        (addPreBuild' preBuild
         (addPostBuild' postBuild pkg)))));

  # how to perform TH for different host platforms.
  doTemplateHaskell = pkg:
    with pkgs.stdenv;
      if hostPlatform.isWindows
      then doTemplateHaskellMingw32 pkg
      else assert buildPlatform == hostPlatform; pkg;

  appendPatchMingw = pkg: p:
    with pkg.stdenv;
      if hostPlatform.isWindows
      then appendPatch pkg p
      else pkg;
}
