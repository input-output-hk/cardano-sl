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
  stagingWalletdockerImage = (import fixedNixpkgs { config = {}; }).runCommand "${iohkPkgs.dockerImages.stagingWallet.name}-hydra" {} ''
    mkdir -pv $out/nix-support/
    cat <<EOF > $out/nix-support/hydra-build-products
    file dockerimage ${iohkPkgs.dockerImages.stagingWallet}
    EOF
  '';
  platforms = {
    cardano-sl = supportedSystems;
    cardano-sl-auxx = supportedSystems;
    cardano-sl-node-static = supportedSystems;
    cardano-sl-tools = supportedSystems;
    cardano-sl-wallet = supportedSystems;
    cardano-sl-explorer-static = [ "x86_64-linux" ];
    cardano-report-server-static = [ "x86_64-linux" ];
    stack2nix = supportedSystems;
    purescript = supportedSystems;
    connectScripts.mainnetWallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.mainnetExplorer = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.stagingWallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.stagingExplorer = [ "x86_64-linux" "x86_64-darwin" ];
  };
in (mapTestOn platforms) // {
  inherit stagingWalletdockerImage;
}
