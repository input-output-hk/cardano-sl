{ haskellPackages, runCommand }:

let
  ghc = haskellPackages.ghcWithPackages (p: with p; [ megaparsec aeson utf8-string ]);
in runCommand "check-logs" { buildInputs = [ ghc ]; } ''
  mkdir -p $out/bin/
  cp -vi ${./.}/*.hs .
  ghc check-logs.hs -o $out/bin/check-logs
''
