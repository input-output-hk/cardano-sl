{ runCommand }:

runCommand "cardano-sl-config" {} ''
  mkdir -p $out/lib
  cp -R ${../log-configs} $out/log-configs
  cp ${../lib}/configuration.yaml $out/lib
  cp ${../lib}/*genesis*.json $out/lib
''
