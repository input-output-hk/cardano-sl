# This is a demo frontend for faucet for testing purposes.

{ lib, runCommand, gnused
, apiBaseURL ? ""
, explorerURL ? "http://cardano-explorer/"
, recaptchaSiteKey ? null
}:

let
  template = ./index.html;
  replaceAPIBaseURL = "-e 's|apiBaseURL:.*|apiBaseURL: \"${apiBaseURL}\",|g'";
  replaceExplorerURL = "-e 's|explorerURL:.*|explorerURL: \"${explorerURL}\",|g'";
  replaceSiteKey = if recaptchaSiteKey != null
    then "-e 's|recaptchaSiteKey:.*|recaptchaSiteKey: \"${recaptchaSiteKey}\",|g'"
    else "";

in
  runCommand "cardano-sl-faucet-frontend" {} ''
    mkdir -p $out
    ${gnused}/bin/sed \
      ${replaceAPIBaseURL} \
      ${replaceExplorerURL} \
      ${replaceSiteKey} \
      ${template} > $out/index.html
  ''
