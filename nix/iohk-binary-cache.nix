# This is a sample NixOS configuration file which you can import into
# your own configuration.nix in order to enable the IOHK binary cache.

{ config, lib, pkgs, ... }:

{
  nix = {
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    binaryCaches = [ "https://hydra.iohk.io" ];
  };
}
