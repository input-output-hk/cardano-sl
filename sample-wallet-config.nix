# If any customization is required, copy this file to
# ./custom-wallet-config.nix and make edits there.

{
  ## NOTE for docker users: The IP's in walletListen and ekgListen
  ## should be set to 0.0.0.0. If you override the port, you must
  ## specify the `-p` parameter to expose the service.

  ## Wallet API server.
  #walletListen = "127.0.0.1:8090";

  ## Wallet Doc API server.
  #walletDocListen = "127.0.0.1:8091";

  ## Runtime metrics server.
  #ekgListen = "127.0.0.1:8000";

  ## Disable client certificate authentication
  #disableClientAuth = true;

  ## Use Legacy Data Layer
  ## The Legacy data layer can be used if v0 API calls are still required.
  ## This will have a negative impact on performance if enabled.
  #useLegacyDataLayer = true;

  ## Directory for the wallet's local state. Must be set BEFORE
  ## running nix-build to have any effect, and it must be enclosed in
  ## double quotes.
  #stateDir = "./state-wallet-mainnet";

  ## Used to connect to a custom set of nodes on the network. When
  ## unspecified an appropriate default topology is generated.
  #topologyFile = ./topology.yaml;

  ## See https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/runtime_control.html#running-a-compiled-program
  #ghcRuntimeArgs = "-N2 -qg -A1m -I0 -T";

  ## Primarily used for troubleshooting.
  #additionalNodeArgs = "";

  ## TLS configuration
  # tlsConfig = {
  #   organization     = "Company Name";
  #
  #   caCommonName     = "Company Name Self-Signed Root CA";
  #   caEexpiryDays    = 3650;
  #
  #   serverCommonName = "Company Name Wallet Node";
  #   serverExpiryDays = 365;
  #   serverAltDNS     = [
  #     "localhost"
  #     "localhost.localdomain"
  #     "127.0.0.1"
  #     "::1"
  #     ## WARNING: in case the wallet node is accessed through port forwarding,
  #     ##          the externally-visible address MUST be provided in this list
  #     ##          (not as the first entry, if it's an IP address).
  #   ];
  #   ## Set this entry if you only want to add entries to the AltDNS list,
  #   ## instead of overriding it completely:
  #   serverAltDNSExtra = [
  #     "8.8.8.8"
  #   ];
  #
  #   clientCommonName = "Company Name Wallet Node Client";
  #   clientExpiryDays = 365;
  # };
}
