{ system ? builtins.currentSystem
, config ? {}
, iohkPkgs ? import ../. { inherit config system; }
, pkgs ? iohkPkgs.pkgs

# Update this if you need a package version recently uploaded to hackage.
# Any timestamp works.
, hackageSnapshot ? "2018-07-17T09:58:14Z"
}:

with pkgs;

let
  deps = [ nixStable coreutils git findutils cabal2nix glibcLocales stack cabal-install iohkPkgs.stack2nix ];
in
  writeScript "generate.sh" ''
    #!${stdenv.shell}
    set -euo pipefail
    export PATH=${lib.makeBinPath deps}
    export HOME=$(pwd)

    echo "Using hackage snapshot from ${hackageSnapshot}"

    # Ensure that nix 1.11 (used by cabal2nix) works in multi-user mode.
    if [ ! -w /nix/store ]; then
        export NIX_REMOTE=''${NIX_REMOTE:-daemon}
    fi

    function cleanup {
      rm -rf hfsevents.nix.new default.nix.new
    }
    trap cleanup EXIT

    # https://github.com/NixOS/cabal2nix/issues/146
    cabal2nix --system x86_64-darwin --revision 25a53d417d7c7a8fc3116b63e3ba14ca7c8f188f \
      https://github.com/luite/hfsevents.git > hfsevents.nix.new
    mv hfsevents.nix.new hfsevents.nix

    # Generate cardano-sl package set
    stack2nix --platform x86_64-linux --hackage-snapshot "${hackageSnapshot}" -j8 --test --bench --no-indent ./.. > default.nix.new
    mv default.nix.new default.nix

    # Generate cabal new-freeze file from package set
    cp --remove-destination --no-preserve=mode \
        $(nix-build ../shell.nix -A cabalProjectFreeze --no-out-link) \
        ../cabal.project.freeze
  ''
