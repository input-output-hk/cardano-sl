/* Fetch pinned nixpkgs without external dependencies on existing nixpkgs
   To be superseeded in Nix 1.12 with fetchTarball having a hash argument
   to avoid re-downloading the tarball.
*/
let
  # fetch nixpkgs and give the expected hash
  unpackTar = input: let
    nixcfg = import <nix/config.nix>;
    script = ''
      export PATH=$coreutils
      $coreutils/mkdir $out
      cd $out
      $gzip -d < $input | $tar -x --strip-components=1
    '';
  in builtins.derivation {
    system = builtins.currentSystem;
    name = "${input.name}-unpacked";
    builder = nixcfg.shell;
    args = [ (builtins.toFile "builder" script) ];
    inherit input;
    coreutils = builtins.storePath nixcfg.coreutils;
    tar = builtins.storePath nixcfg.tar;
    gzip = builtins.storePath nixcfg.gzip;
  };
  fetchNixpkgsTarWithNix = let
    spec = builtins.fromJSON (builtins.readFile ./nixpkgs-src.json);
  in import <nix/fetchurl.nix> {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  };
in
  unpackTar fetchNixpkgsTarWithNix
