{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, bytestring, cardano-sl-core, cardano-sl-crypto, cardano-sl-update
, cardano-sl-util, cardano-sl-wallet, cardano-sl-wallet-new
, connection, cryptonite, data-default, directory, ekg, ekg-core
, ekg-statsd, exceptions, filepath, http-api-data, http-client
, http-client-tls, lens, log-warper, memory, mmorph, mtl
, neat-interpolation, optparse-applicative, QuickCheck, random
, serokell-util, servant, servant-client, servant-client-core
, servant-server, servant-swagger, servant-swagger-ui, stdenv
, swagger2, tagged, text, text-format, time, tls, wai, wai-cors
, wai-extra, warp, wreq
}:
mkDerivation {
  pname = "cardano-sl-faucet";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base base16-bytestring bytestring
    cardano-sl-core cardano-sl-crypto cardano-sl-update cardano-sl-util
    cardano-sl-wallet cardano-sl-wallet-new connection cryptonite
    data-default directory ekg-core ekg-statsd exceptions filepath
    http-api-data http-client http-client-tls lens log-warper memory
    mmorph mtl neat-interpolation QuickCheck random serokell-util
    servant servant-client servant-client-core servant-server
    servant-swagger servant-swagger-ui swagger2 tagged text text-format
    time tls wreq
  ];
  executableHaskellDepends = [
    aeson base bytestring cardano-sl-core cardano-sl-update
    cardano-sl-util cardano-sl-wallet cardano-sl-wallet-new ekg
    ekg-core ekg-statsd exceptions lens log-warper mmorph mtl
    optparse-applicative servant servant-client servant-server text wai
    wai-cors wai-extra warp
  ];
  testHaskellDepends = [ base cardano-sl-wallet QuickCheck ];
  license = stdenv.lib.licenses.mit;
}
