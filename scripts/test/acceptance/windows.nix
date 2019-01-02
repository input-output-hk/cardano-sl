with import ../../../lib.nix;

{ stdenv, runCommand, zip
, cardano-sl-tools, cardano-sl-wallet, cardano-sl-wallet-tool
, cardano-sl-config

# Network to run tests against
, environment
}:

let
  deps = [ cardano-sl-tools cardano-sl-wallet cardano-sl-wallet-tool ];

in
  runCommand "acceptance-tests-${environment}.zip" {} ''
    cp -Rv --no-preserve=mode ${cardano-sl-config}/* .

    ${concatMapStringsSep "\n" (dep: "cp -v ${dep}/bin/* .") deps}

    # fixme: need to copy some dlls

    ${zip}/bin/zip -r $out *
  ''
