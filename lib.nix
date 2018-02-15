let
  # Allow overriding pinned nixpkgs for debugging purposes via cardano_pkgs
  fetchNixPkgs = let try = builtins.tryEval <cardano_pkgs>;
    in if try.success
    then builtins.trace "using host <cardano_pkgs>" try.value
    else import ./fetch-nixpkgs.nix;

  maybeEnv = env: default:
    let
      result = builtins.getEnv env;
    in if result != ""
       then result
       else default;

  pkgs = import fetchNixPkgs {};
  lib = pkgs.lib;
in lib // (rec {
  inherit fetchNixPkgs;
  ifThenElse = { bool, thenValue, elseValue }: (if bool then thenValue else elseValue);
  fetchFromGitHub = ifThenElse {
    bool = (0 <= builtins.compareVersions builtins.nixVersion "1.12pre");
    thenValue = { owner, repo, rev, sha256 }: builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    };
    elseValue = pkgs.fetchFromGitHub;
  };
})
