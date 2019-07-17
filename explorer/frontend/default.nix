{ pkgs, stdenv, runCommand, writeScriptBin, fetchzip
, buildBowerComponents
, cardano-sl-explorer, gitrev
}:

with import ../../lib.nix;

let
  # Removes files within a Haskell source tree which won't change the
  # result of building the package.
  # This is so that cached build products can be used whenever possible.
  # It also applies the lib.cleanSource filter from nixpkgs which
  # removes VCS directories, emacs backup files, etc.
  cleanSourceTree = src:
    if (builtins.typeOf src) == "path"
      then cleanSourceWith {
        filter = with pkgs.stdenv;
          name: type: let baseName = baseNameOf (toString name); in ! (
            # Filter out cabal build products.
            baseName == "dist" || baseName == "dist-newstyle" ||
            baseName == "cabal.project.local" ||
            lib.hasPrefix ".ghc.environment" baseName ||
            # Filter out stack build products.
            lib.hasPrefix ".stack-work" baseName ||
            # Filter out files which are commonly edited but don't
            # affect the cabal build.
            lib.hasSuffix ".nix" baseName
          );
        src = cleanSource src;
      } else src;
  src = cleanSourceWith {
    src = cleanSourceTree ./.;
    filter = with stdenv;
      name: type: let baseName = baseNameOf (toString name); in ! (
        # Filter out locally generated/downloaded things.
        baseName == "bower_components" || baseName == "node_modules"
      );
  };

  bowerComponents = buildBowerComponents {
    name = "cardano-sl-explorer-frontend-deps";
    generated = ./nix/bower-generated.nix;
    inherit src;
  };

  # p-d-l does not build with our main version of nixpkgs.
  # Needs to use something off 17.03 branch.
  oldHaskellPackages = (import (fetchzip {
    url = "https://github.com/NixOS/nixpkgs/archive/cb90e6a0361554d01b7a576af6c6fae4c28d7513.tar.gz";
    sha256 = "0gr25nph2yyk89j2g5zxqm2177lkh0cyy8gnzm6xcywz1qwf3zzf";
  }) {}).pkgs.haskell.packages.ghc802.override {
    overrides = self: super: {
      purescript-derive-lenses = oldHaskellPackages.callPackage ./nix/purescript-derive-lenses.nix {};
    };
  };

  yarn2nix = import (fetchzip {
    url = "https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz";
    sha256 = "02bzr9j83i1064r1r34cn74z7ccb84qb5iaivwdplaykyyydl1k8";
  }) {
    # nixpkgs 18.03 branch contains yarn 1.5.1
    inherit pkgs;
    nodejs = pkgs.nodejs-6_x;
  };

  regen-script = writeScriptBin "regen" ''
    export PATH=${makeBinPath [oldHaskellPackages.purescript-derive-lenses cardano-sl-explorer]}:$PATH
    cardano-explorer-hs2purs --bridge-path src/Generated/
    scripts/generate-explorer-lenses.sh
  '';

  frontend = { stdenv, python, mkYarnPackage }:
    mkYarnPackage {
      name = "cardano-explorer-frontend";
      inherit src;
      yarnLock = ./yarn.lock;
      packageJSON = ./package.json;
      extraBuildInputs = [
        oldHaskellPackages.purescript-derive-lenses
        cardano-sl-explorer
        oldHaskellPackages.purescript
        regen-script
      ];
      passthru = { inherit bowerComponents; };
      postConfigure = ''
        rm -rf .psci_modules .pulp-cache bower_components output result

        # Purescript code generation
        regen

        # Frontend dependencies
        ln -s ${bowerComponents}/bower_components .

        # Patch the build recipe for nix
        echo "patching webpack.config.babel.js"
        sed -e "s/COMMIT_HASH.*/COMMIT_HASH': '\"@GITREV@\"',/" \
            -e "s/import GitRevisionPlugin.*//" \
            -e "s/path:.*/path: process.env.out,/" \
            -e "/new ProgressPlugin/d" \
            -i webpack.config.babel.js
      '';
      installPhase = ''
        # run the build:prod script
        export PATH=$(pwd)/node_modules/.bin:$PATH
        export NODE_ENV=production
        export HOME=$(pwd)/webpack-home
        webpack --config webpack.config.babel.js
      '';
    };

  # Stamps the frontend with the git revision in a way that avoids
  # a webpack rebuild when the git revision changes.
  # This will just replace @GITREV@ in all javascript files.
  # See also: cardano-sl/scripts/set-git-rev/default.nix
  withGitRev = drvOut: let
    drvOutOutputs = drvOut.outputs or ["out"];
  in
    runCommand drvOut.name {
      outputs  = drvOutOutputs;
      passthru = drvOut.drvAttrs
        // (drvOut.passthru or {})
        // { inherit gitrev; };
    }
    (concatMapStrings (output: ''
      cp -a "${drvOut.${output}}" "${"$"}${output}"
      chmod -R +w "${"$"}${output}"
      find "${"$"}${output}" -type f -name '*.js' \
        -exec echo Setting gitrev in {} ';' \
        -exec sed -i 's/@GITREV@/${gitrev}/g' {} ';'
    '') drvOutOutputs);

in

  withGitRev (pkgs.callPackage frontend {
    inherit (yarn2nix) mkYarnPackage;
  })
