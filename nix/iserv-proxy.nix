{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "iserv-proxy";
        version = "8.5";
      };
      license = "BSD-3-Clause";
      copyright = "XXX";
      maintainer = "XXX";
      author = "XXX";
      homepage = "";
      url = "";
      synopsis = "iserv allows GHC to delegate Tempalte Haskell computations";
      description = "GHC can be provided with a path to the iserv binary with\n@-pgmi=/path/to/iserv-bin@, and will in combination with\n@-fexternal-interpreter@, compile Template Haskell though the\n@iserv-bin@ delegate. This is very similar to how ghcjs has been\ncompiling Template Haskell, by spawning a separate delegate (so\ncalled runner on the javascript vm) and evaluating the splices\nthere.\n\niserv can also be used in combination with cross compilation. For\nthis, the @iserv-proxy@ needs to be built on the host, targeting the\nhost (as it is running on the host). @cabal install -flibrary\n-fproxy@ will yield the proxy.\n\nUsing the cabal for the target @arch-platform-target-cabal install\n-flibrary@ will build the required library that contains the ffi\n@startSlave@ function, which needs to be invoked on the target\n(e.g. in an iOS application) to start the remote iserv slave.\n\ncalling the GHC cross compiler with @-fexternal-interpreter\n-pgmi=\$HOME/.cabal/bin/iserv-proxy -opti\\<ip address\\> -opti\\<port\\>@\nwill cause it to compile Template Haskell via the remote at \\<ip address\\>.\n\nThus to get cross compilation with Template Haskell follow the\nfollowing receipt:\n\n* compile the iserv library for your target\n\n> iserv \$ arch-platform-target-cabal install -flibrary\n\n* setup an application for your target that calls the\n* startSlave function. This could be either haskell or your\n* targets ffi capable language, if needed.\n\n>  void startSlave(false /* verbose */, 5000 /* port */,\n>                  \"/path/to/storagelocation/on/target\");\n\n* build the iserv-proxy\n\n> iserv \$ cabal install -flibrary -fproxy\n* Start your iserv-slave app on your target running on say @10.0.0.1:5000@\n* compiler your sources with -fexternal-interpreter and the proxy\n\n> project \$ arch-platform-target-ghc ModuleContainingTH.hs \\\n>             -fexternal-interpreter \\\n>             -pgmi=\$HOME/.cabal/bin/iserv-proxy \\\n>             -opti10.0.0.1 -opti5000\n\nShould something not work as expected, provide @-opti-v@ for verbose\nlogging of the @iserv-proxy@.";
      buildType = "Simple";
    };
    components = {
      exes = {
        "iserv-proxy" = {
          depends  = [
            (hsPkgs.array)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.ghci)
            (hsPkgs.directory)
            (hsPkgs.network)
            (hsPkgs.filepath)
            (hsPkgs.libiserv)
          ];
        };
      };
    };
  } // rec { src = pkgs.fetchurl { url = https://s3.eu-central-1.amazonaws.com/ci-static/ghc-cross-windows/iserv-proxy-8.4.4.tar.gz; sha256 = "121mxf3575d9rh7z1nhahx0g2ch7pxqbr6fvy7557ddqn49w2114"; }; }
