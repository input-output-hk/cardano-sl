# Imports the iohk-nix library.
# The version can be overridden for debugging purposes by setting
# NIX_PATH=iohk_nix=/path/to/iohk-nix
import (
  let try = builtins.tryEval <iohk_nix>;
  in if try.success
  then builtins.trace "using host <iohk_nix>" try.value
  else
    let
      spec = builtins.fromJSON (builtins.readFile ./iohk-nix-src.json);
    in builtins.fetchTarball {
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
      inherit (spec) sha256;
    }) {}
