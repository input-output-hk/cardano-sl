{ args ? { config = import ./config.nix; }
, nixpkgs ? import <nixpkgs>
}:
let
  pkgs = nixpkgs args;
  overrideWith = override: default:
   let
     try = builtins.tryEval (builtins.findFile builtins.nixPath override);
   in if try.success then
     builtins.trace "using search host <${override}>" try.value
   else
     default;

in
let
  # save the nixpkgs value in pkgs'
  # so we can work with `pkgs` provided by modules.
  pkgs' = pkgs;
  #
  nixpkgs' = import (import ./fetch-nixpkgs.nix) {};
  # all packages from hackage as nix expressions
  hackage = import (overrideWith "hackage"
                    (nixpkgs'.fetchFromGitHub { owner  = "angerman";
                                                repo   = "hackage.nix";
                                                rev    = "d8e03ec0e3c99903d970406ae5bceac7d993035d";
                                                sha256 = "0c7camspw7v5bg23hcas0r10c40fnwwwmz0adsjpsxgdjxayws3v";
                                                name   = "hackage-exprs-source"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (nixpkgs'.fetchFromGitHub { owner  = "angerman";
                                                repo   = "haskell.nix";
                                                rev    = "03026b7bb95a6713f4d50b841abadabb343f83d2";
                                                sha256 = "05ma2qmmn4p2xcgyy8waissfj953b7wyq97yx80d936074gyyw4s";
                                                name   = "haskell-lib-source"; }))
                   hackage;

  # the set of all stackage snapshots
  stackage = import (overrideWith "stackage"
                     (nixpkgs'.fetchFromGitHub { owner  = "angerman";
                                                 repo   = "stackage.nix";
                                                 rev    = "67675ea78ae5c321ed0b8327040addecc743a96c";
                                                 sha256 = "1ds2xfsnkm2byg8js6c9032nvfwmbx7lgcsndjgkhgq56bmw5wap";
                                                 name   = "stackage-snapshot-source"; }))
                   ;

  # our packages
  stack-pkgs = import ./.stack-pkgs.nix;

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  pkgSet = haskell.mkNewPkgSet {
    inherit pkgs;
    pkg-def = stackage.${stack-pkgs.resolver};
    pkg-def-overlays = [
      stack-pkgs.overlay
      # We use some customized libiserv/remote-iserv/iserv-proxy
      # instead of the ones provided by ghc. This is mostly due
      # to being able to hack on them freely as needed.
      #
      # iserv is only relevant for template-haskell execution in
      # a cross compiling setup.
      {
        ghci         = ./ghci.nix;
        ghc-boot     = ./ghc-boot.nix;
        libiserv     = ./libiserv.nix;
        remote-iserv = ./remote-iserv.nix;
        iserv-proxy  = ./iserv-proxy.nix;
#        packages.hfsevents.revision = import ../hfsevents-0.1.6;
      }
      (hackage: {
          hsc2hs = hackage.hsc2hs."0.68.4".revisions.default;
          # stackage 12.17 beautifully omitts the Win32 pkg
          Win32 = hackage.Win32."2.6.2.0".revisions.default;
      })
    ];
    modules = [
      {
         # This needs true, otherwise we miss most of the interesting
         # modules.
         packages.ghci.flags.ghci = true;
         # this needs to be true to expose module
         #  Message.Remote
         # as needed by libiserv.
         packages.libiserv.flags.network = true;

         # enable golden tests on cardano-crypto
         packages.cardano-crypto.flags.golden-tests = true;
      }
      ({ config, ... }: {
          packages.hsc2hs.components.exes.hsc2hs.doExactConfig= true;
          packages.Win32.components.library.build-tools = [ config.hsPkgs.buildPackages.hsc2hs ];
#          packages.Win32.components.library.doExactConfig = true;
          packages.remote-iserv.postInstall = ''
            cp ${pkgs.windows.mingw_w64_pthreads}/bin/libwinpthread-1.dll $out/bin/
          '';
      })
      {
        packages.conduit.patches            = [ ./patches/conduit-1.3.0.2.patch ];
        packages.cryptonite-openssl.patches = [ ./patches/cryptonite-openssl-0.7.patch ];
        packages.streaming-commons.patches  = [ ./patches/streaming-commons-0.2.0.0.patch ];
        packages.x509-system.patches        = [ ./patches/x509-system-1.6.6.patch ];
        packages.file-embed-lzma.patches    = [ ./patches/file-embed-lzma-0.patch ];

        packages.cardano-sl.patches         = [ ./patches/cardano-sl.patch ];
      }
      # cross compilation logic
      ({ pkgs, buildModules, config, lib, ... }:
      let
        withTH = import ./mingw_w64.nix {
          inherit (pkgs') stdenv lib writeScriptBin;
          wine = pkgs.buildPackages.winePackages.minimal;
          inherit (pkgs.windows) mingw_w64_pthreads;
          inherit (pkgs) gmp;
          # iserv-proxy needs to come from the buildPackages, as it needs to run on the
          # build host.
          inherit (config.hsPkgs.buildPackages.iserv-proxy.components.exes) iserv-proxy;
          # remote-iserv however needs to come from the regular packages as it has to
          # run on the target host.
          inherit (packages.remote-iserv.components.exes) remote-iserv;
          # we need to use openssl.bin here, because the .dll's are in the .bin expression.
          extra-test-libs = [ pkgs.rocksdb pkgs.openssl.bin ];

        } // { doCrossCheck = true; };
       in lib.optionalAttrs pkgs'.stdenv.hostPlatform.isWindows  {
         packages.generics-sop      = withTH;
         packages.ether             = withTH;
         packages.th-lift-instances = withTH;
         packages.aeson             = withTH;
         packages.hedgehog          = withTH;
         packages.th-orphans        = withTH;
         packages.uri-bytestring    = withTH;
         packages.these             = withTH;
         packages.katip             = withTH;
         packages.swagger2          = withTH;
         packages.wreq              = withTH;
         packages.wai-app-static    = withTH;
         packages.log-warper        = withTH;
         packages.cardano-sl-util   = withTH;
         packages.cardano-sl-crypto = withTH;
         packages.cardano-sl-crypto-test = withTH;
         packages.cardano-sl-core   = withTH;
         packages.cardano-sl        = withTH;
         packages.cardano-sl-chain  = withTH;
         packages.cardano-sl-db     = withTH;
         packages.cardano-sl-networking = withTH;
         packages.cardano-sl-infra  = withTH;
         packages.cardano-sl-infra-test = withTH;
         packages.cardano-sl-client = withTH;
         packages.cardano-sl-core-test = withTH;
         packages.cardano-sl-chain-test = withTH;
         packages.cardano-sl-utxo   = withTH;
         packages.cardano-wallet = withTH;
         packages.cardano-sl-tools    = withTH;
         packages.cardano-sl-generator = withTH;
         packages.cardano-sl-auxx     = withTH;
         packages.cardano-sl-faucet   = withTH;
         packages.cardano-sl-binary   = withTH;
         packages.cardano-sl-node     = withTH;
         packages.cardano-sl-explorer = withTH;
         packages.cardano-sl-cluster  = withTH;
         packages.cardano-sl-x509     = withTH;
         packages.cardano-sl-mnemonic = withTH;
         packages.cardano-crypto      = withTH;
         packages.math-functions    = withTH;
         packages.servant-swagger-ui = withTH;
         packages.servant-swagger-ui-redoc = withTH;
         packages.trifecta            = withTH;
         packages.Chart               = withTH;
         packages.active              = withTH;
         packages.diagrams            = withTH;
         packages.diagrams-lib        = withTH;
         packages.diagrams-svg        = withTH;
         packages.diagrams-postscript = withTH;
         packages.Chart-diagrams      = withTH;
         packages.statistics          = withTH;
      })
      # packages we wish to ignore version bounds of.
      # this is similar to jailbreakCabal, however it
      # does not require any messing with cabal files.
      {
         packages.katip.components.library.doExactConfig         = true;
         packages.serokell-util.components.library.doExactConfig = true;
         # trutle wants Win32 < 2.6
         packages.turtle.components.library.doExactConfig        = true;
      }
      ({ pkgs, ... }: {
         packages.hfsevents.components.library.frameworks  = [ pkgs.CoreServices ];
      })
    ];
  };

  packages = pkgSet.config.hsPkgs // { _config = pkgSet.config; };

in packages
