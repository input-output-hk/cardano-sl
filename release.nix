let
  fixedLib     = import ./lib.nix;
  fixedNixpkgs = fixedLib.fetchNixPkgs;
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
  , cardano ? { outPath = ./.; rev = "abcdef"; }
  , fasterBuild ? false
  , skipDocker ? false
  , skipPackages ? []
  , nixpkgsArgs ? {
      config = (import ./nix/config.nix // { allowUnfree = false; inHydra = true; });
      gitrev = cardano.rev;
      inherit fasterBuild;
    }
  }:

with (import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import ./.;
});

let
  iohkPkgs = import ./. { gitrev = cardano.rev; };
  pkgs = import fixedNixpkgs { config = {}; };
  shellEnv = import ./shell.nix { };
  wrapDockerImage = cluster: let
    images = {
      mainnet = iohkPkgs.dockerImages.mainnet;
      staging = iohkPkgs.dockerImages.staging;
    };
    wrapImage = image: pkgs.runCommand "${image.name}-hydra" {} ''
      mkdir -pv $out/nix-support/
      cat <<EOF > $out/nix-support/hydra-build-products
      file dockerimage ${image}
      EOF
    '';
  in {
    wallet = wrapImage images."${cluster}".wallet;
    explorer = wrapImage images."${cluster}".explorer;
  };
  platforms = removeAttrs {
    all-cardano-sl = supportedSystems;
    cardano-report-server = [ "x86_64-linux" ];
    cardano-report-server-static = [ "x86_64-linux" ];
    cardano-sl = supportedSystems;
    cardano-sl-auxx = supportedSystems;
    cardano-sl-chain = supportedSystems;
    cardano-sl-cluster = [ "x86_64-linux" ];
    cardano-sl-core = supportedSystems;
    cardano-sl-crypto = supportedSystems;
    cardano-sl-db = supportedSystems;
    cardano-sl-explorer = [ "x86_64-linux" ];
    cardano-sl-explorer-frontend = [ "x86_64-linux" ];
    cardano-sl-explorer-static = [ "x86_64-linux" ];
    cardano-sl-generator = supportedSystems;
    cardano-sl-infra = supportedSystems;
    cardano-sl-networking = supportedSystems;
    cardano-sl-node-static = supportedSystems;
    cardano-sl-tools = supportedSystems;
    cardano-sl-tools-post-mortem = supportedSystems;
    cardano-sl-util = supportedSystems;
    cardano-sl-x509 = supportedSystems;
    cardano-wallet = supportedSystems;
    daedalus-bridge = supportedSystems;
    shells.cabal = supportedSystems;
    shells.stack = supportedSystems;
    stack2nix = supportedSystems;

  } skipPackages;
  platforms' = removeAttrs {
    connectScripts.mainnet.wallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.mainnet.explorer = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.staging.wallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.staging.explorer = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.testnet.wallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.testnet.explorer = [ "x86_64-linux" "x86_64-darwin" ];
  } skipPackages;
  mapped = mapTestOn platforms;

  nix-tools-toolchain = supportedSystems: {
    nix-tools.libs = removeAttrs {
      # nix-tools toolchain: Libraries
      cardano-sl            = supportedSystems;
      cardano-sl-auxx       = supportedSystems;
      cardano-sl-chain      = supportedSystems;
      cardano-sl-core       = supportedSystems;
      cardano-sl-crypto     = supportedSystems;
      cardano-sl-db         = supportedSystems;
      cardano-sl-generator  = supportedSystems;
      cardano-sl-infra      = supportedSystems;
      cardano-sl-networking = supportedSystems;
      cardano-sl-tools      = supportedSystems;
      cardano-sl-util       = supportedSystems;
      cardano-wallet        = supportedSystems;
      cardano-sl-x509       = supportedSystems;
      turtle                = supportedSystems;
    } skipPackages;
    nix-tools.exes = removeAttrs {
      # nix-tools toolchain: Executables
      # these will usually implicitly build their
      # library as they depend on it.
      cardano-sl-tools             = supportedSystems;
      cardano-sl-generator         = supportedSystems;
      cardano-sl-tools-post-mortem = supportedSystems;
      cardano-wallet               = supportedSystems;
    } skipPackages;
    # nix-tools toolchain: Tests
    nix-tools.tests =
      removeAttrs
        (lib.mapAttrs (_: lib.mapAttrs (_: _: supportedSystems))
          (lib.filterAttrs (n: v: fixedLib.isCardanoSL n && v != null)
            iohkPkgs.nix-tools.tests))
        skipPackages;
    # nix-tools toolchain: Benchmarks
    nix-tools.benchmarks =
      removeAttrs
        (lib.mapAttrs (_: lib.mapAttrs (_: _: supportedSystems))
          (lib.filterAttrs (n: v: fixedLib.isCardanoSL n && v != null)
            iohkPkgs.nix-tools.benchmarks))
        skipPackages;
  };

  # tests that are either broken or broken on some arch
  broken-tests           = {
        # these two tests are broken on darwin and take forever.
        # thus we only test them on linux
        cardano-sl-chain.chain-test = [ "x86_64-linux" ];
        cardano-wallet.unit = [ "x86_64-linux" ];
  };
  broken-tests-cross     = {
        # This one does not complete on wine.
        cardano-sl-db.db-test = [];
        cardano-sl-auxx.cardano-auxx-test = [ "x86_64-linux" ];
        cardano-sl-client.cardano-client-test = [ "x86_64-linux" ];
        cardano-sl-cluster.cardano-sl-cluster-test = [ "x86_64-linux" ];
        cardano-sl-explorer.cardano-explorer-test = [ "x86_64-linux" ];
        cardano-sl-faucet.faucet-test = [ "x86_64-linux" ];
        cardano-sl-generator.cardano-generator-test = [ "x86_64-linux" ];
        cardano-sl-mnemonic.cardano-sl-mnemonic-test = [ "x86_64-linux" ];
        cardano-sl-node.property-tests = [ "x86_64-linux" ];
        cardano-sl.cardano-test = [ "x86_64-linux" ];
        cardano-sl-x509.cardano-sl-x509-test = [ "x86_64-linux" ];
        cardano-sl-tools.cardano-sl-tools-test = [ "x86_64-linux" ];
        cardano-sl-networking.cardano-sl-networking-test = [ "x86_64-linux" ];
        cardano-sl-infra.infra-test = [ "x86_64-linux" ];
        cardano-sl-crypto.crypto-test = [ "x86_64-linux" ];
        cardano-crypto.cardano-crypto-test = [ "x86_64-linux" ];
        cardano-crypto.cardano-crypto-golden-tests = [ "x86_64-linux" ];
        cardano-sl-core.core-test = [ "x86_64-linux" ];
        cardano-sl-chain.chain-test = [ "x86_64-linux" ];
        cardano-sl-binary.binary-test = [ "x86_64-linux" ];
        cardano-sl-util.util-test = [ "x86_64-linux" ];
        cardano-wallet.nightly = [ "x86_64-linux" ];
        cardano-wallet.unit = [ "x86_64-linux" ];
        cardano-report-server.cardano-report-server-test = [ "x86_64-linux" ];
  };
  mapped-nix-tools       = mapTestOn                                    (lib.recursiveUpdate (nix-tools-toolchain supportedSystems)   { nix-tools.tests = broken-tests; });
  # do linux->windows only, not mac->windows.
  mapped-nix-tools-cross = mapTestOnCross lib.systems.examples.mingwW64 (lib.recursiveUpdate (nix-tools-toolchain [ "x86_64-linux" ]) { nix-tools.tests = broken-tests-cross; });

  # prefix the targets with their triple.
  #
  #  x86_64-pc-mingw32-$pkg
  #
  mapped-nix-tools'
    = (lib.recursiveUpdate
        (mapped-nix-tools)
        (lib.mapAttrs (_: (lib.mapAttrs (_: (lib.mapAttrs' (n: v: lib.nameValuePair (lib.systems.examples.mingwW64.config + "-" + n) v)))))
          mapped-nix-tools-cross))
      // {
        daedalus-mingw32-pkg = pkgs.runCommand "daedalus-mingw32-pkg" {} ''
          mkdir -p daedalus $out

          cd daedalus
          cp ${mapped-nix-tools-cross.nix-tools.exes.cardano-wallet.x86_64-linux}/bin/cardano-node.exe .
          cp ${mapped-nix-tools-cross.nix-tools.exes.cardano-sl-tools.x86_64-linux}/bin/cardano-launcher.exe .
          cp ${mapped-nix-tools-cross.nix-tools.exes.cardano-sl-tools.x86_64-linux}/bin/wallet-extractor.exe .
          cp ${mapped-nix-tools-cross.nix-tools.exes.cardano-sl-tools.x86_64-linux}/bin/cardano-x509-certificates.exe .
          cp ${./log-configs/daedalus.yaml} log-config-prod.yaml
          cp ${./lib/configuration.yaml} configuration.yaml
          cp ${./lib}/*genesis*.json .
          echo ${cardano.rev} > commit-id
          ${pkgs.zip}/bin/zip -9 $out/CardanoSL.zip *

          # add CardanoSL.zip to the hydra-build-products to make
          # hydra provide a link to it.
          mkdir -p $out/nix-support
          echo "file binary-dist \"$out/CardanoSL.zip\"" \
              > $out/nix-support/hydra-build-products
        '';
      };

  mapped' = mapTestOn platforms';
  makeConnectScripts = cluster: let
  in {
    inherit (mapped'.connectScripts."${cluster}") wallet explorer;
  };
  nixosTests = import ./nixos-tests;
  tests = iohkPkgs.tests;
  makeRelease = cluster: {
    name = cluster;
    value = {
      connectScripts = makeConnectScripts cluster;
    } // fixedLib.optionalAttrs (! skipDocker) {
      dockerImage = wrapDockerImage cluster;
    };
  };
  # return an attribute set containing the result of running every test-suite in cardano, on the given system
  makeCardanoTestRuns = system:
  let
    pred = name: value: fixedLib.isCardanoSL name && value ? testrun;
    cardanoPkgs = import ./. { inherit system; };
    f = name: value: value.testrun;
  in pkgs.lib.mapAttrs f (lib.filterAttrs pred cardanoPkgs);
in pkgs.lib.fix (jobsets: mapped // mapped-nix-tools' // {
  inherit tests;
  inherit (pkgs) cabal2nix;
  nixpkgs = let
    wrapped = pkgs.runCommand "nixpkgs" {} ''
      ln -sv ${fixedNixpkgs} $out
    '';
  in if 0 <= builtins.compareVersions builtins.nixVersion "1.12" then wrapped else fixedNixpkgs;
  regen-script = import ./pkgs/regen.nix {};
  # the result of running every cardano test-suite on 64bit linux
  all-cardano-tests.x86_64-linux = makeCardanoTestRuns "x86_64-linux";
  # hydra will create a special aggregate job, that relies on all of these sub-jobs passing
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "cardano-required-checks";
    constituents =
      let
        allLinux = x: map (system: x.${system}) [ "x86_64-linux" ];
        all = x: map (system: x.${system}) supportedSystems;
        recursiveFlattenAttrs = set: builtins.concatLists (lib.mapAttrsFlatten (key: value: if (lib.isDerivation value) then [value] else (recursiveFlattenAttrs value)) set);
      in
    [
      (builtins.concatLists (map lib.attrValues (allLinux jobsets.all-cardano-tests)))
      (all jobsets.all-cardano-sl)
      (all jobsets.daedalus-bridge)
      jobsets.mainnet.connectScripts.wallet.x86_64-linux
      jobsets.tests.hlint
      jobsets.tests.shellcheck
      jobsets.tests.stylishHaskell
      jobsets.tests.swaggerSchemaValidation
      (recursiveFlattenAttrs jobsets.nix-tools.benchmarks)
      (builtins.concatLists (lib.attrValues (lib.mapAttrs (_: allLinux) jobsets.nix-tools.libs)))
      (builtins.concatLists (lib.attrValues (lib.mapAttrs (_: allLinux) jobsets.nix-tools.exes)))
    ];
  });
}
// (builtins.listToAttrs (map makeRelease [ "mainnet" "staging" ])))
