let
  fixedNixpkgs = (import ./lib.nix).fetchNixPkgs;
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" "win64" ]
  , targetSystemsNoCross ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
  , cardano ? { outPath = ./.; rev = "abcdef"; }
  , nixpkgsArgs ? {
      config = spaces: (import ./config.nix spaces)
             // { allowUnfree = false; inHydra = true; };
      gitrev = cardano.rev;
    }
  }:

let
  iohkPkgs = import ./. { gitrev = cardano.rev; };
  pkgs = import fixedNixpkgs { config = _: {}; };
  lib = pkgs.lib;
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
    all-cardano-sl = supportedSystems;
    cardano-sl-explorer-static = [ "x86_64-linux" ];
    cardano-sl-explorer-frontend = [ "x86_64-linux" ];
    cardano-report-server-static = [ "x86_64-linux" ];
    stack2nix = targetSystemsNoCross;
    purescript = targetSystemsNoCross;
    daedalus-bridge = supportedSystems;
  };
  platforms' = {
    connectScripts.mainnet.wallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.mainnet.explorer = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.staging.wallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.staging.explorer = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.testnet.wallet   = [ "x86_64-linux" "x86_64-darwin" ];
    connectScripts.testnet.explorer = [ "x86_64-linux" "x86_64-darwin" ];
  };
  mapTestOn' = let
    func = import ./.;
    pkgs_linux = func (nixpkgsArgs // { system = "x86_64-linux"; });
    pkgs_mac = func (nixpkgsArgs // { system = "x86_64-darwin"; });
    pkgs_win = func (nixpkgsArgs // { target = "win64"; });
    f = path: value: let
        maybeLinux = lib.optionalAttrs (builtins.elem "x86_64-linux" value) ({ "x86_64-linux" = lib.getAttrFromPath path pkgs_linux; });
        maybeMac = lib.optionalAttrs (builtins.elem "x86_64-darwin" value) ({ "x86_64-darwin" = lib.getAttrFromPath path pkgs_mac; });
        maybeWin = lib.optionalAttrs (builtins.elem "win64" value) ({ win64 = lib.getAttrFromPath path pkgs_win; });
      in maybeLinux // maybeMac // maybeWin;
  in set: lib.mapAttrsRecursive f set;
  mapped = mapTestOn' platforms;
  mapped' = mapTestOn' platforms';
  makeConnectScripts = cluster: let
  in {
    inherit (mapped'.connectScripts."${cluster}") wallet explorer;
  };
  nixosTests = import ./nixos-tests;
  tests = iohkPkgs.tests;
  makeRelease = cluster: {
    name = cluster;
    value = {
      dockerImage = wrapDockerImage cluster;
      connectScripts = makeConnectScripts cluster;
    };
  };
in mapped // {
  inherit tests;
  nixpkgs = let
    wrapped = pkgs.runCommand "nixpkgs" {} ''
      ln -sv ${fixedNixpkgs} $out
    '';
  in if 0 <= builtins.compareVersions builtins.nixVersion "1.12" then wrapped else fixedNixpkgs;
} // (builtins.listToAttrs (map makeRelease [ "mainnet" "staging" ]))
