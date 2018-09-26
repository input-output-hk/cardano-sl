{ enableProfiling ? false }:
let
  iohkPkgs = import ./. { inherit enableProfiling; };
  mapper = k: v: if v ? env then
  v.env.overrideAttrs (drv: {
    buildInputs = (drv.buildInputs or []) ++ [ iohkPkgs.pkgs.haskellPackages.ghcid iohkPkgs.pkgs.haskellPackages.cabal-install ];
  })
  else
    v;
in
  builtins.mapAttrs mapper iohkPkgs
