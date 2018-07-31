{ runCommand, hlint, src, lib }:

let
  # just haskell sources and the hlint config file
  src' = lib.cleanSourceWith {
   inherit src;
   filter = with lib;
    name: type: let baseName = baseNameOf (toString name); in (
      (type == "regular" && hasSuffix ".hs" baseName) ||
      (type == "regular" && hasSuffix ".yaml" baseName) ||
      (type == "directory")
    );
  };
in
runCommand "cardano-hlint-check" { buildInputs = [ hlint ]; } ''
  set +e
  # Networking is not in here, because it has a very different codestyle (doesn't use universum).
  # This is bad and should probably be fixed.
  projects=("util" "binary" "crypto" "core" "db" "chain" "infra" "node" "tools" "client" "generator" "auxx" "explorer" "wallet" "wallet-new")
  cd ${src'}
  hlint lib/src lib/test lib/bench "''${projects[@]}"
  EXIT_CODE=$?
  if [[ $EXIT_CODE != 0 ]]
  then
    echo '====================================================================='
    echo 'Note: to ignore a particular hint (e.g. "Reduce duplication"), write'
    echo 'this in the source file:'
    tput bold
    echo '{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}'
    tput sgr0
    echo 'You can also apply it just to a particular function, which is better:'
    tput bold
    echo '{-# ANN funcName ("HLint: ignore Reduce duplication" :: Text) #-}'
    tput sgr0
    exit $EXIT_CODE
  else
    echo $EXIT_CODE > $out
  fi
''
