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
                                                rev    = "ed1dbc01f98411894f6613c62818f14b02fb6679";
                                                sha256 = "0kbj4kb9rlvjb4afpcisz9zlk5z3h7frkwggfwri1q5683fapkgv";
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

        # https://github.com/biegunka/terminal-size/pull/12
        packages.terminal-size.patches      = [ ./patches/terminal-size-hsc-alignment.patch ];
        packages.scrypt.patches             = [ ./patches/scrypt-scrypt-prefix.patch ];
      }
      ({ lib, ... }: lib.optionalAttrs pkgs'.stdenv.hostPlatform.isMusl {
        packages.rocksdb-haskell-ng.patches = [ ./patches/rocksdb-add-libs.patch ];
      })
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
      })
      ({ lib, ... }:
       lib.optionalAttrs pkgs'.stdenv.hostPlatform.isMusl {
         packages.cardano-wallet.configureFlags = [
           # we really want static.
           "--ghc-option=-optl=-static"
           # and we want also static c++ (due to rocksdb)
           "--ghc-option=-optl=-static-libstdc++"
           "--ghc-option=-optl=-static-libgcc"
           # now, ghc is not really smart and will keep using gcc
           # however gcc is not smart either and will fail over
           # linking c++ libraries, even if we say we want
           # static-libc++. So let's tell ghc to force g++ as
           # a linker. We'll need to fix the
           "--ghc-option=-pgml=${pkgs'.stdenv.hostPlatform.config}-g++"

           # make a bunch of libraries static and add them to the link.
           "--extra-lib-dirs=${pkgs'.gmp6.override { withStatic = true; }}/lib"
           "--extra-lib-dirs=${pkgs'.zlib.static}/lib"
           "--extra-lib-dirs=${pkgs'.static-openssl.openssl_1_0_2.out}/lib"
           "--extra-lib-dirs=${(pkgs'.lzma.overrideAttrs (old: { dontDisableStatic = true; })).out}/lib"
           "--extra-lib-dirs=${pkgs'.rocksdb.static}/lib"
         ];
        # we also patch rocksdb, to pass snappy, lz4, bz2, and
        # jemalloc as dependencies.  This is mostly due to ghc
        # not giving us an -optl flag that appends libraries
        # at the end.  Note: linkers are a bit annoying in that
        # the order of -l flags actually matters.
        #
        # We maybe able to abuse --start-group, but that feels
        # like an even bigger hack.
        packages.rocksdb-haskell-ng.configureFlags = [
           "--extra-lib-dirs=${pkgs'.static-snappy}/lib"
           "--extra-lib-dirs=${pkgs'.static-lz4}/lib"
           "--extra-lib-dirs=${(pkgs'.bzip2.override { linkStatic = true; }).out}/lib"
           "--extra-lib-dirs=${pkgs'.jemalloc}/lib"
           ];
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
