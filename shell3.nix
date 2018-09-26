attr:
{ enableProfiling ? false }:

(import ./shell2.nix { inherit enableProfiling; })."${attr}"
