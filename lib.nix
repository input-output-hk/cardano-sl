let
  # fetch nixpkgs and give the expected hash
  unpackTar = input: let
    nixcfg = import <nix/config.nix>;
    script = ''
      set -x
      export PATH=$coreutils
      mkdir $out
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
  fetchNixPkgs = if builtins.getEnv "NIX_PATH_LOCKED" == "1"
    then builtins.trace "using host <nixpkgs>" <nixpkgs>
    else builtins.trace "fetching nixpkgs"    (unpackTar fetchNixpkgsTarWithNix);
  pkgs = import fetchNixPkgs {};
  lib = pkgs.lib;
in lib // (rec {
  inherit fetchNixPkgs;

  # modulo operator
  # mod 11 10 == 1
  # mod 1 10 == 1
  mod = base: int: base - (int * (builtins.div base int));
})
