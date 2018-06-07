let
  fixedNixpkgs = (import ./lib.nix).fetchNixPkgs;
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
  , cardano ? { outPath = ./.; rev = "abcdef"; }
  , nixpkgsArgs ? {
      config = { allowUnfree = false; inHydra = true; };
      gitrev = cardano.rev;
    }
  }:

with (import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import ./.;
});

let
  iohkPkgs = import ./. { gitrev = cardano.rev; };
  pkgs = import fixedNixpkgs { config = {}; };
  wrapDockerImage = cluster: let
    images = {
      mainnet = iohkPkgs.dockerImages.mainnet.wallet;
      staging = iohkPkgs.dockerImages.staging.wallet;
    };
    image = images."${cluster}";
  in pkgs.runCommand "${image.name}-hydra" {} ''
    mkdir -pv $out/nix-support/
    cat <<EOF > $out/nix-support/hydra-build-products
    file dockerimage ${image}
    EOF
  '';
  platforms = {
    cardano-sl = supportedSystems;
    cardano-sl-auxx = supportedSystems;
    cardano-sl-node-static = supportedSystems;
    cardano-sl-tools = supportedSystems;
    cardano-sl-wallet = supportedSystems;
    cardano-sl-wallet-new = supportedSystems;
    cardano-sl-explorer-static = [ "x86_64-linux" ];
    cardano-report-server-static = [ "x86_64-linux" ];
    stack2nix = supportedSystems;
    purescript = supportedSystems;
    daedalus-bridge = supportedSystems;
  };
  platforms' = {
    connectScripts.mainnet.wallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.mainnet.explorer = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.staging.wallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.staging.explorer = [ "x86_64-linux" "x86_64-darwin" ];
  };
  mapped = mapTestOn platforms;
  mapped' = mapTestOn platforms';
  makeConnectScripts = cluster: let
  in {
    inherit (mapped'.connectScripts."${cluster}") wallet explorer;
  };
  nixosTests = import ./nixos-tests;
  shellcheckTests = iohkPkgs.shellcheckTests;
  swaggerSchemaValidation = iohkPkgs.swaggerSchemaValidation;
  walletIntegrationTests = iohkPkgs.buildWalletIntegrationTests;
  makeRelease = cluster: {
    name = cluster;
    value = {
      dockerImage = wrapDockerImage cluster;
      connectScripts = makeConnectScripts cluster;
    };
  };
in mapped // {
  inherit walletIntegrationTests swaggerSchemaValidation shellcheckTests;
  nixpkgs = let
    wrapped = pkgs.runCommand "nixpkgs" {} ''
      ln -sv ${fixedNixpkgs} $out
    '';
  in if 0 <= builtins.compareVersions builtins.nixVersion "1.12" then wrapped else fixedNixpkgs;
} // (builtins.listToAttrs (map makeRelease [ "mainnet" "staging" ]))
