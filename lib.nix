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
  isCardanoSL = lib.hasPrefix "cardano-sl";
  isBenchmark = args: !((args.isExecutable or false) || (args.isLibrary or true));

  # Insert this into builder scripts where programs require a UTF-8
  # locale to work.
  utf8LocaleSetting = ''
    export LC_ALL=en_GB.UTF-8
    export LANG=en_GB.UTF-8
  '' + lib.optionalString (pkgs.glibcLocales != null) ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '';
})
