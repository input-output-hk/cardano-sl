{ pkgs ? import <nixpkgs> {}
}:
let
  overrideWith = override: default:
   let
     try = builtins.tryEval (builtins.findFile builtins.nixPath override);
   in if try.success then
     builtins.trace "using search host <${override}>" try.value
   else
     default;
in
let
  # all packages from hackage as nix expressions
  hackage = import (overrideWith "hackage"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "hackage.nix";
                                            rev    = "20ad44ecdd5475c8adbe0e129638f729a26ca120";
                                            sha256 = "0bh0p58i9w9nw2mcjgx6j9qyi6x5xg8pn5x37a696kw1bgwm8wzn"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "haskell.nix";
                                            rev    = "3b2cff33565e31e31a8a33eb5ebfa20a19aa70d6";
                                            sha256 = "1hjqppxh9vmvlfbfpkg7gcijjhq4hhlx4xah87ma0w1nw7vk7nda"; }))
                   hackage;

  # the set of all stackage snapshots
  stackage = import (overrideWith "stackage"
                     (pkgs.fetchFromGitHub { owner  = "angerman";
                                             repo   = "stackage.nix";
                                             rev    = "385609120d6f20a67f79e5120a93b4524d8c8862";
                                             sha256 = "1l3k5qpbj6w2mg6rgmg0af2jk0bq1wwrijrn66grbw7kbbi4h9nx"; }))
                    { inherit pkgs hackage haskell; };
  # our packages
  stack-pkgs = import ./nix/stack-pkgs.nix;

  # pick the repsective stackage version here
  # and augment them with out packages
  stackPackages = stackage.${stack-pkgs.resolver} {
    extraDeps = hsPkgs: (stack-pkgs.extraDeps hsPkgs
                      // stack-pkgs.packages  hsPkgs); };
in stackPackages
