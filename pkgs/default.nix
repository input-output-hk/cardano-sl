# Generated using stack2nix 0.1.3.0.
#
# Only works with sufficiently recent nixpkgs, e.g. "NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/21a8239452adae3a4717772f4e490575586b2755.tar.gz".

{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc802
, ghc ? pkgs.haskell.compiler.ghc802
}:

with pkgs.haskell.lib;

let
  stackPackages = { callPackage, pkgs, stdenv }:
self: {
      Cabal = callPackage ({ array, base, binary, bytestring, containers, deepseq, directory, filepath, mkDerivation, pretty, process, stdenv, time, unix }:
      mkDerivation {
          pname = "Cabal";
          version = "1.24.2.0";
          sha256 = "b7d0eb8e3503fbca460c0a6ca5c88352cecfe1b69e0bbc79827872134ed86340";
          revision = "2";
          editedCabalFile = "15ncrm7x2lg4hn0m5mhc8hy769bzhmajsm6l9i6536plfs2bbbdj";
          libraryHaskellDepends = [
            array
            base
            binary
            bytestring
            containers
            deepseq
            directory
            filepath
            pretty
            process
            time
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.haskell.org/cabal/";
          description = "A framework for packaging Haskell software";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      Chart = callPackage ({ array, base, colour, data-default-class, lens, mkDerivation, mtl, old-locale, operational, stdenv, time, vector }:
      mkDerivation {
          pname = "Chart";
          version = "1.8.2";
          sha256 = "8442c16959e2a46355418b82c0c6fc3174d04b41ea6e2e320c56588a563be28d";
          libraryHaskellDepends = [
            array
            base
            colour
            data-default-class
            lens
            mtl
            old-locale
            operational
            time
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/timbod7/haskell-chart/wiki";
          description = "A library for generating 2D Charts and Plots";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      Chart-diagrams = callPackage ({ Chart, SVGFonts, base, blaze-markup, bytestring, colour, containers, data-default-class, diagrams-core, diagrams-lib, diagrams-postscript, diagrams-svg, lens, mkDerivation, mtl, old-locale, operational, stdenv, svg-builder, text, time }:
      mkDerivation {
          pname = "Chart-diagrams";
          version = "1.8.2";
          sha256 = "ca181dec04bac1029101dd75951f48710ebc42f5333e06c57943e3245bba9f41";
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            base
            blaze-markup
            bytestring
            Chart
            colour
            containers
            data-default-class
            diagrams-core
            diagrams-lib
            diagrams-postscript
            diagrams-svg
            lens
            mtl
            old-locale
            operational
            svg-builder
            SVGFonts
            text
            time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/timbod7/haskell-chart/wiki";
          description = "Diagrams backend for Charts";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      Glob = callPackage ({ base, containers, directory, dlist, filepath, mkDerivation, stdenv, transformers, transformers-compat }:
      mkDerivation {
          pname = "Glob";
          version = "0.8.0";
          sha256 = "38f011be0e7818ab1e76880882b15217cd7d5be56a3dab631c14d614e2b2e896";
          libraryHaskellDepends = [
            base
            containers
            directory
            dlist
            filepath
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://iki.fi/matti.niemenmaa/glob/";
          description = "Globbing library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      HUnit = callPackage ({ base, call-stack, deepseq, mkDerivation, stdenv }:
      mkDerivation {
          pname = "HUnit";
          version = "1.5.0.0";
          sha256 = "65c51d17ced1c0646d888cd8caf195df67f6fdc1394c34459bcfd1be0f9ddea0";
          libraryHaskellDepends = [
            base
            call-stack
            deepseq
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hspec/HUnit#readme";
          description = "A unit testing framework for Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      JuicyPixels = callPackage ({ base, binary, bytestring, containers, deepseq, mkDerivation, mtl, primitive, stdenv, transformers, vector, zlib }:
      mkDerivation {
          pname = "JuicyPixels";
          version = "3.2.8.2";
          sha256 = "b74c89e57ea81f6f69470dce215beccb7ced270ce8529b4a97535879ea5478e8";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            containers
            deepseq
            mtl
            primitive
            transformers
            vector
            zlib
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/Twinside/Juicy.Pixels";
          description = "Picture loading/serialization (in png, jpeg, bitmap, gif, tga, tiff and radiance)";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      MonadRandom = callPackage ({ base, fail, mkDerivation, mtl, primitive, random, stdenv, transformers, transformers-compat }:
      mkDerivation {
          pname = "MonadRandom";
          version = "0.5.1";
          sha256 = "9e3f0f92807285302036dc504066ae6d968c8b0b4c25d9360888f31fe1730d87";
          libraryHaskellDepends = [
            base
            fail
            mtl
            primitive
            random
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          description = "Random-number generation monad";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      OneTuple = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "OneTuple";
          version = "0.2.1";
          sha256 = "4b6f74b6d92df112b0f4eaf15ccdc5fbb763d59f07e9a2afa5690ef89159a2f4";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Singleton Tuple";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      QuickCheck = callPackage ({ base, containers, mkDerivation, random, stdenv, template-haskell, tf-random, transformers }:
      mkDerivation {
          pname = "QuickCheck";
          version = "2.9.2";
          sha256 = "155c1656f583bc797587846ee1959143d2b1b9c88fbcb9d3f510f58d8fb93685";
          libraryHaskellDepends = [
            base
            containers
            random
            template-haskell
            tf-random
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nick8325/quickcheck";
          description = "Automatic testing of Haskell programs";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      SHA = callPackage ({ array, base, binary, bytestring, directory, mkDerivation, stdenv }:
      mkDerivation {
          pname = "SHA";
          version = "1.6.4.2";
          sha256 = "c470176f63cbe49fd0502a1b32ef22bc01b1af42385583b8be94547750958a8c";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            array
            base
            binary
            bytestring
          ];
          executableHaskellDepends = [
            base
            bytestring
            directory
          ];
          doHaddock = false;
          doCheck = false;
          description = "Implementations of the SHA suite of message digest functions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      SVGFonts = callPackage ({ attoparsec, base, blaze-markup, blaze-svg, bytestring, cereal, cereal-vector, containers, data-default-class, diagrams-core, diagrams-lib, directory, mkDerivation, parsec, split, stdenv, text, tuple, vector, xml }:
      mkDerivation {
          pname = "SVGFonts";
          version = "1.6.0.1";
          sha256 = "f727ef24f8591c2d6aea64d85c569db56db5324093dcf569d417ac6b1582d0f0";
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            attoparsec
            base
            blaze-markup
            blaze-svg
            bytestring
            cereal
            cereal-vector
            containers
            data-default-class
            diagrams-core
            diagrams-lib
            directory
            parsec
            split
            text
            tuple
            vector
            xml
          ];
          doHaddock = false;
          doCheck = false;
          description = "Fonts from the SVG-Font format";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      StateVar = callPackage ({ base, mkDerivation, stdenv, stm, transformers }:
      mkDerivation {
          pname = "StateVar";
          version = "1.1.0.4";
          sha256 = "7ad68decb5c9a76f83c95ece5fa13d1b053e4fb1079bd2d3538f6b05014dffb7";
          libraryHaskellDepends = [
            base
            stm
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-opengl/StateVar";
          description = "State variables";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      abstract-deque = callPackage ({ array, base, containers, mkDerivation, random, stdenv, time }:
      mkDerivation {
          pname = "abstract-deque";
          version = "0.3";
          sha256 = "09aa10f38193a8275a7791b92a4f3a7192a304874637e2a35c897dde25d75ca2";
          libraryHaskellDepends = [
            array
            base
            containers
            random
            time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/rrnewton/haskell-lockfree/wiki";
          description = "Abstract, parameterized interface to mutable Deques";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      abstract-par = callPackage ({ base, deepseq, mkDerivation, stdenv }:
      mkDerivation {
          pname = "abstract-par";
          version = "0.3.3";
          sha256 = "248a8739bd902462cb16755b690b55660e196e58cc7e6ef8157a72c2a3d5d860";
          libraryHaskellDepends = [
            base
            deepseq
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/simonmar/monad-par";
          description = "Type classes generalizing the functionality of the 'monad-par' library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      acid-state = callPackage ({ array, base, bytestring, cereal, containers, directory, extensible-exceptions, fetchgit, filepath, mkDerivation, mtl, network, safecopy, stdenv, stm, template-haskell, th-expand-syns, unix }:
      mkDerivation {
          pname = "acid-state";
          version = "0.14.2";
          src = fetchgit {
            url = "https://github.com/serokell/acid-state.git";
            sha256 = "109liqzk66cxkarw8r8jxh27n6qzdcha2xlhsj56xzyqc2aqjz15";
            rev = "95fce1dbada62020a0b2d6aa2dd7e88eadd7214b";
          };
          libraryHaskellDepends = [
            array
            base
            bytestring
            cereal
            containers
            directory
            extensible-exceptions
            filepath
            mtl
            network
            safecopy
            stm
            template-haskell
            th-expand-syns
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://acid-state.seize.it/";
          description = "Add ACID guarantees to any serializable Haskell data structure";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      active = callPackage ({ base, lens, linear, mkDerivation, semigroupoids, semigroups, stdenv, vector }:
      mkDerivation {
          pname = "active";
          version = "0.2.0.13";
          sha256 = "5d9a141d58bcefbf699ed233a22309ded671c25ed64bcee11a663d00731280fb";
          revision = "2";
          editedCabalFile = "1ml42hbvfhqzpdi1y5q6dqp4wq6zqb30f15r34n9ip9iv44qjwwf";
          libraryHaskellDepends = [
            base
            lens
            linear
            semigroupoids
            semigroups
            vector
          ];
          doHaddock = false;
          doCheck = false;
          description = "Abstractions for animation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      adjunctions = callPackage ({ array, base, comonad, containers, contravariant, distributive, free, mkDerivation, mtl, profunctors, semigroupoids, semigroups, stdenv, tagged, transformers, transformers-compat, void }:
      mkDerivation {
          pname = "adjunctions";
          version = "4.3";
          sha256 = "b948a14fafe8857f451ae3e474f5264c907b5a2d841d52bf78249ae4749c3ecc";
          revision = "1";
          editedCabalFile = "1079l9szyr7ybi9wcvv1vjsjfrqirkn9z3j7dann8vbk81a4z37q";
          libraryHaskellDepends = [
            array
            base
            comonad
            containers
            contravariant
            distributive
            free
            mtl
            profunctors
            semigroupoids
            semigroups
            tagged
            transformers
            transformers-compat
            void
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/adjunctions/";
          description = "Adjunctions and representable functors";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      aeson = callPackage ({ attoparsec, base, base-compat, bytestring, containers, deepseq, dlist, ghc-prim, hashable, mkDerivation, scientific, stdenv, tagged, template-haskell, text, time, time-locale-compat, unordered-containers, uuid-types, vector }:
      mkDerivation {
          pname = "aeson";
          version = "1.1.2.0";
          sha256 = "37488cfbf6ecf65c4d63164d760c1a0f3bcc3371a35a50e5c4a3c0fd2ffac5ff";
          revision = "1";
          editedCabalFile = "06acsik1qcn5r1z1y3n7iw5h8x0h3hdcjii0bq9nf9ncvc71h1d4";
          libraryHaskellDepends = [
            attoparsec
            base
            base-compat
            bytestring
            containers
            deepseq
            dlist
            ghc-prim
            hashable
            scientific
            tagged
            template-haskell
            text
            time
            time-locale-compat
            unordered-containers
            uuid-types
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/aeson";
          description = "Fast JSON parsing and encoding";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      aeson-pretty = callPackage ({ aeson, attoparsec, base, base-compat, bytestring, cmdargs, mkDerivation, scientific, stdenv, text, unordered-containers, vector }:
      mkDerivation {
          pname = "aeson-pretty";
          version = "0.8.5";
          sha256 = "dd17e86c64b3fe2efb7a855b27b0e5490e42dc58194ae1809d8b662d4e42a9f9";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            base
            base-compat
            bytestring
            scientific
            text
            unordered-containers
            vector
          ];
          executableHaskellDepends = [
            aeson
            attoparsec
            base
            bytestring
            cmdargs
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/informatikr/aeson-pretty";
          description = "JSON pretty-printing library and command-line tool";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ansi-terminal = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "ansi-terminal";
          version = "0.6.3.1";
          sha256 = "458f98e0c9217897f0ff07f730cfc3ed380089936fb31942aec31bb336608095";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
          ];
          executableHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/feuerbach/ansi-terminal";
          description = "Simple ANSI terminal support, with Windows compatibility";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ansi-wl-pprint = callPackage ({ ansi-terminal, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "ansi-wl-pprint";
          version = "0.6.7.3";
          sha256 = "3789ecaa89721eabef58ddc5711f7fd1ff67e262da1659f3b20d38a9e1f5b708";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            ansi-terminal
            base
          ];
          executableHaskellDepends = [
            ansi-terminal
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/ansi-wl-pprint";
          description = "The Wadler/Leijen Pretty Printer for colored ANSI terminal output";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      appar = callPackage ({ base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "appar";
          version = "0.1.4";
          sha256 = "58ea66abe4dd502d2fc01eecdb0828d5e214704a3c1b33b1f8b33974644c4b26";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          description = "A simple applicative parser";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      array = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "array";
          version = "0.5.1.1";
          sha256 = "89c96958578da5051f684e38dacad7558ec023a7b08f97eb19876dba08ce2223";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Mutable and immutable arrays";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      asn1-encoding = callPackage ({ asn1-types, base, bytestring, hourglass, mkDerivation, stdenv }:
      mkDerivation {
          pname = "asn1-encoding";
          version = "0.9.5";
          sha256 = "1e863bfd363f6c3760cc80f2c0d422e17845a9f79fe006030db202ecab5aaf29";
          libraryHaskellDepends = [
            asn1-types
            base
            bytestring
            hourglass
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-asn1";
          description = "ASN1 data reader and writer in RAW, BER and DER forms";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      asn1-parse = callPackage ({ asn1-encoding, asn1-types, base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "asn1-parse";
          version = "0.9.4";
          sha256 = "c6a328f570c69db73f8d2416f9251e8a03753f90d5d19e76cbe69509a3ceb708";
          libraryHaskellDepends = [
            asn1-encoding
            asn1-types
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/vincenthz/hs-asn1";
          description = "Simple monadic parser for ASN1 stream types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      asn1-types = callPackage ({ base, bytestring, hourglass, memory, mkDerivation, stdenv }:
      mkDerivation {
          pname = "asn1-types";
          version = "0.3.2";
          sha256 = "0c571fff4a10559c6a630d4851ba3cdf1d558185ce3dcfca1136f9883d647217";
          libraryHaskellDepends = [
            base
            bytestring
            hourglass
            memory
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-asn1-types";
          description = "ASN.1 types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      async = callPackage ({ base, mkDerivation, stdenv, stm }:
      mkDerivation {
          pname = "async";
          version = "2.1.1.1";
          sha256 = "cd83e471466ea6885b2e8fb60f452db3ac3fdf3ea2d6370aa1e071ebc37544e2";
          libraryHaskellDepends = [
            base
            stm
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/simonmar/async";
          description = "Run IO operations asynchronously and wait for their results";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      attoparsec = callPackage ({ array, base, bytestring, containers, deepseq, mkDerivation, scientific, stdenv, text, transformers }:
      mkDerivation {
          pname = "attoparsec";
          version = "0.13.1.0";
          sha256 = "52dc74d4955e457ce4f76f5c9d6dba05c1d07e2cd2a542d6251c6dbc66ce3f64";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
            deepseq
            scientific
            text
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/attoparsec";
          description = "Fast combinator parsing for bytestrings and text";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      attoparsec-iso8601 = callPackage ({ attoparsec, base, base-compat, mkDerivation, stdenv, text, time }:
      mkDerivation {
          pname = "attoparsec-iso8601";
          version = "1.0.0.0";
          sha256 = "aa6c6d87587383e386cb85e7ffcc4a6317aa8dafb8ba9a104ecac365ce2a858a";
          libraryHaskellDepends = [
            attoparsec
            base
            base-compat
            text
            time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/aeson";
          description = "Parsing of ISO 8601 dates, originally from aeson";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      auto-update = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "auto-update";
          version = "0.1.4";
          sha256 = "5e96c151024e8bcaf4eaa932e16995872b2017f46124b967e155744d9580b425";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/yesodweb/wai";
          description = "Efficiently run periodic, on-demand actions";
          license = stdenv.lib.licenses.mit;
        }) {};
      autoexporter = callPackage ({ Cabal, base, directory, filepath, mkDerivation, stdenv }:
      mkDerivation {
          pname = "autoexporter";
          version = "1.1.2";
          sha256 = "9dcefc3c3c1299c29345c0df43a471fc854c546b78140f81a064adf1fbfdf7d8";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            Cabal
            directory
            filepath
          ];
          executableHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/tfausak/autoexporter#readme";
          description = "Automatically re-export modules";
          license = stdenv.lib.licenses.mit;
        }) {};
      base = callPackage ({ ghc-prim, integer-gmp, mkDerivation, rts, stdenv }:
      mkDerivation {
          pname = "base";
          version = "4.9.1.0";
          sha256 = "7a5b85805f06f869ca86f99e12cb098c611ab623dd70305ca2b389823d71fb7e";
          libraryHaskellDepends = [
            ghc-prim
            integer-gmp
            rts
          ];
          doHaddock = false;
          doCheck = false;
          description = "Basic libraries";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      base-compat = callPackage ({ base, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "base-compat";
          version = "0.9.3";
          sha256 = "7d602b0f0543fadbd598a090c738e9ce9b07a1896673dc27f1503ae3bea1a210";
          libraryHaskellDepends = [
            base
            unix
          ];
          doHaddock = false;
          doCheck = false;
          description = "A compatibility layer for base";
          license = stdenv.lib.licenses.mit;
        }) {};
      base-orphans = callPackage ({ base, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "base-orphans";
          version = "0.6";
          sha256 = "c7282aa7516652e6e4a78ccdfb654a99c9da683875748ad5898a3f200be7ad0e";
          libraryHaskellDepends = [
            base
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-compat/base-orphans#readme";
          description = "Backwards-compatible orphan instances for base";
          license = stdenv.lib.licenses.mit;
        }) {};
      base-prelude = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "base-prelude";
          version = "1.2.0.1";
          sha256 = "811a494f5996ff1012be15a1236cc4afb6a67fc2a9f54fdb53f4e94a8fde119e";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nikita-volkov/base-prelude";
          description = "The most complete prelude formed solely from the \"base\" package";
          license = stdenv.lib.licenses.mit;
        }) {};
      base-unicode-symbols = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "base-unicode-symbols";
          version = "0.2.2.4";
          sha256 = "a2f841430fec32edba778b74bde83bf0170ada7c5e2e59d7187c8f06d92dcca9";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell.org/haskellwiki/Unicode-symbols";
          description = "Unicode alternatives for common functions and operators";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      base16-bytestring = callPackage ({ base, bytestring, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "base16-bytestring";
          version = "0.1.1.6";
          sha256 = "5afe65a152c5418f5f4e3579a5e0d5ca19c279dc9bf31c1a371ccbe84705c449";
          libraryHaskellDepends = [
            base
            bytestring
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/bos/base16-bytestring";
          description = "Fast base16 (hex) encoding and decoding for ByteStrings";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      base58-bytestring = callPackage ({ base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "base58-bytestring";
          version = "0.1.0";
          sha256 = "c2dbf598f3415053e12cca84b90fa7c0c1b02f3b784cce0157264baebf2d40d3";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://bitbucket.org/s9gf4ult/base58-bytestring";
          description = "Implementation of BASE58 transcoding for ByteStrings";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      base64-bytestring = callPackage ({ base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "base64-bytestring";
          version = "1.0.0.1";
          sha256 = "ab25abf4b00a2f52b270bc3ed43f1d59f16c8eec9d7dffb14df1e9265b233b50";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/base64-bytestring";
          description = "Fast base64 encoding and decoding for ByteStrings";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      bifunctors = callPackage ({ base, base-orphans, comonad, containers, mkDerivation, semigroups, stdenv, tagged, template-haskell, transformers, transformers-compat }:
      mkDerivation {
          pname = "bifunctors";
          version = "5.4.2";
          sha256 = "38620267824abbf834f708f1b7cf10307c1d2719b1a0f8ae49330a1002dfdc8d";
          libraryHaskellDepends = [
            base
            base-orphans
            comonad
            containers
            semigroups
            tagged
            template-haskell
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/bifunctors/";
          description = "Bifunctors";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      binary = callPackage ({ array, base, bytestring, containers, mkDerivation, stdenv }:
      mkDerivation {
          pname = "binary";
          version = "0.8.3.0";
          sha256 = "221385dde77d92f786c665ee6fce0a3beeb80e6a812b8edf9ded1b653f2ea821";
          revision = "2";
          editedCabalFile = "0nz3v9pq1jy72j4drahjx055xhjj47yncanjsfgpphcmch9yl26i";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/kolmodin/binary";
          description = "Binary serialisation for Haskell values using lazy ByteStrings";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      blaze-builder = callPackage ({ base, bytestring, deepseq, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "blaze-builder";
          version = "0.4.0.2";
          sha256 = "9ad3e4661bf5556d650fb9aa56a3ad6e6eec7575e87d472e8ab6d15eaef163d4";
          libraryHaskellDepends = [
            base
            bytestring
            deepseq
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/lpsmith/blaze-builder";
          description = "Efficient buffered output";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      blaze-html = callPackage ({ base, blaze-builder, blaze-markup, bytestring, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "blaze-html";
          version = "0.9.0.1";
          sha256 = "aeceaab3fbccbf7f01a241819e6c16c0a1cf19dccecb795c5de5407bc8660a64";
          libraryHaskellDepends = [
            base
            blaze-builder
            blaze-markup
            bytestring
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://jaspervdj.be/blaze";
          description = "A blazingly fast HTML combinator library for Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      blaze-markup = callPackage ({ base, blaze-builder, bytestring, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "blaze-markup";
          version = "0.8.0.0";
          sha256 = "19e1cbb9303803273ed7f9fcf3b8b6938578afbed2bfafe5ea9fcc6d743f540f";
          libraryHaskellDepends = [
            base
            blaze-builder
            bytestring
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://jaspervdj.be/blaze";
          description = "A blazingly fast markup combinator library for Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      blaze-svg = callPackage ({ base, blaze-markup, mkDerivation, mtl, stdenv }:
      mkDerivation {
          pname = "blaze-svg";
          version = "0.3.6.1";
          sha256 = "f6a4f1bba1e973b336e94de73369f4562778fde43b6ac7c0b32d6a501527aa60";
          libraryHaskellDepends = [
            base
            blaze-markup
            mtl
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/deepakjois/blaze-svg";
          description = "SVG combinator library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      byteable = callPackage ({ base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "byteable";
          version = "0.1.1";
          sha256 = "243b34a1b5b64b39e39fe58f75c18f6cad5b668b10cabcd86816cbde27783fe2";
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-byteable";
          description = "Type class for sequence of bytes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      byteorder = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "byteorder";
          version = "1.0.4";
          sha256 = "bd20bbb586947f99c38a4c93d9d0266f49f6fc581767b51ba568f6d5d52d2919";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://community.haskell.org/~aslatter/code/byteorder";
          description = "Exposes the native endianness or byte ordering of the system";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      bytes = callPackage ({ Cabal, base, binary, bytestring, cabal-doctest, cereal, containers, hashable, mkDerivation, mtl, scientific, stdenv, text, time, transformers, transformers-compat, unordered-containers, void }:
      mkDerivation {
          pname = "bytes";
          version = "0.15.3";
          sha256 = "d8dcd6b66492db37e48b95535cf3bf91b1b0f356fedba403eb73f81158e0cd4d";
          revision = "2";
          editedCabalFile = "07j20bmhysp4dawy8am1j4lhg21s5c2i8ckqby0iykmfgrlsrcv0";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            base
            binary
            bytestring
            cereal
            containers
            hashable
            mtl
            scientific
            text
            time
            transformers
            transformers-compat
            unordered-containers
            void
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ekmett/bytes";
          description = "Sharing code for serialization between binary and cereal";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      bytestring = callPackage ({ base, deepseq, ghc-prim, integer-gmp, mkDerivation, stdenv }:
      mkDerivation {
          pname = "bytestring";
          version = "0.10.8.1";
          sha256 = "2d615f5b4cd76251663ea67355589742950590bf766e487961fbfc816e58fc9b";
          libraryHaskellDepends = [
            base
            deepseq
            ghc-prim
            integer-gmp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/bytestring";
          description = "Fast, compact, strict and lazy byte strings with a list interface";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      bytestring-builder = callPackage ({ base, bytestring, deepseq, mkDerivation, stdenv }:
      mkDerivation {
          pname = "bytestring-builder";
          version = "0.10.8.1.0";
          sha256 = "6d7404773621efb88b256ff88912a7dbcebc7fb86d27868ef58478249892dbc2";
          libraryHaskellDepends = [
            base
            bytestring
            deepseq
          ];
          doHaddock = false;
          doCheck = false;
          description = "The new bytestring builder, packaged outside of GHC";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cabal-doctest = callPackage ({ Cabal, base, directory, filepath, mkDerivation, stdenv }:
      mkDerivation {
          pname = "cabal-doctest";
          version = "1.0.2";
          sha256 = "4a1b8cdfcca9cd1e3dcb0afca4fefeb348c8be4d0eb0be7fe013bd2a9cd47c40";
          libraryHaskellDepends = [
            base
            Cabal
            directory
            filepath
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/cabal-doctest";
          description = "A Setup.hs helper for doctests running";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      call-stack = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "call-stack";
          version = "0.1.0";
          sha256 = "f25f5e0992a39371079cc25c2a14b5abb872fa7d868a32753aac3a258b83b1e2";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/sol/call-stack#readme";
          description = "Use GHC call-stacks in a backward compatible way";
          license = stdenv.lib.licenses.mit;
        }) {};
      canonical-json = callPackage ({ base, bytestring, containers, fetchgit, mkDerivation, parsec, pretty, stdenv }:
      mkDerivation {
          pname = "canonical-json";
          version = "0.5.0.0";
          src = fetchgit {
            url = "https://github.com/well-typed/canonical-json.git";
            sha256 = "19lc5pr85jz3f8ifmjxnkxgib0lz3vgagdny50gb04midc7y37pr";
            rev = "2d261bb971bada1893753b503452d9e6e217bc4a";
          };
          libraryHaskellDepends = [
            base
            bytestring
            containers
            parsec
            pretty
          ];
          doHaddock = false;
          doCheck = false;
          description = "Canonical JSON for signing and hashing JSON values";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cardano-crypto = callPackage ({ base, bytestring, cryptonite, cryptonite-openssl, deepseq, fetchgit, hashable, memory, mkDerivation, stdenv }:
      mkDerivation {
          pname = "cardano-crypto";
          version = "1.0.0";
          src = fetchgit {
            url = "https://github.com/input-output-hk/cardano-crypto";
            sha256 = "10f89zm2sd015r6fbhlk1zp0720rzq2dvwazrmcxa3bd5s2l696v";
            rev = "1cde8e3a8d9093bbf571085920045c05edb3eaa4";
          };
          libraryHaskellDepends = [
            base
            bytestring
            cryptonite
            cryptonite-openssl
            deepseq
            hashable
            memory
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/input-output-hk/cardano-crypto#readme";
          description = "Cryptography primitives for cardano";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-report-server = callPackage ({ aeson, aeson-pretty, base, bytestring, case-insensitive, directory, exceptions, fetchgit, filelock, filepath, formatting, http-types, lens, lifted-base, log-warper, mkDerivation, monad-control, mtl, network, optparse-applicative, parsec, random, stdenv, text, time, transformers, universum, vector, wai, wai-extra, warp }:
      mkDerivation {
          pname = "cardano-report-server";
          version = "0.3.0";
          src = fetchgit {
            url = "https://github.com/input-output-hk/cardano-report-server.git";
            sha256 = "0kysicb6ij4mwkg8dx222hn1lxzalmzb79z1f9bpm6dfjhs7m0sf";
            rev = "69583b607dd841b0de1ef660388172a94c660c84";
          };
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            aeson-pretty
            base
            bytestring
            case-insensitive
            directory
            exceptions
            filelock
            filepath
            formatting
            http-types
            lens
            lifted-base
            log-warper
            monad-control
            mtl
            network
            optparse-applicative
            parsec
            random
            text
            time
            transformers
            universum
            vector
            wai
            wai-extra
            warp
          ];
          executableHaskellDepends = [
            base
            directory
            filepath
            http-types
            log-warper
            monad-control
            mtl
            optparse-applicative
            parsec
            random
            universum
            wai-extra
            warp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/input-output-hk/cardano-report-server";
          description = "Reporting server for CSL";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cardano-sl = callPackage ({ MonadRandom, QuickCheck, acid-state, aeson, ansi-terminal, ansi-wl-pprint, async, base, base58-bytestring, base64-bytestring, binary, bytestring, canonical-json, cardano-crypto, cardano-report-server, cardano-sl-core, cardano-sl-db, cardano-sl-godtossing, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, cardano-sl-txp, cardano-sl-update, cborg, cereal, conduit, containers, cpphs, cryptonite, cryptonite-openssl, data-default, deepseq, deriving-compat, digest, directory, dlist, dns, ed25519, ekg-core, ekg-statsd, ekg-wai, ether, exceptions, file-embed, filelock, filepath, focus, formatting, generic-arbitrary, hashable, hspec, http-client, http-client-tls, http-conduit, http-types, iproute, kademlia, lens, list-t, log-warper, lrucache, memory, mkDerivation, mmorph, monad-control, monad-loops, mono-traversable, mtl, neat-interpolation, network-info, network-transport, network-transport-tcp, network-uri, node-sketch, optparse-applicative, parsec, plutus-prototype, pvss, quickcheck-instances, random, reflection, regex-tdfa, regex-tdfa-text, resourcet, rocksdb-haskell, safe-exceptions, safecopy, serokell-util, servant, servant-multipart, servant-server, stdenv, stm, stm-containers, string-qq, systemd, tagged, template-haskell, text, text-format, th-lift-instances, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers, vector, wai, wai-extra, warp, warp-tls, yaml }:
      mkDerivation {
          pname = "cardano-sl";
          version = "1.0.3";
          src = ./../node;
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            acid-state
            aeson
            ansi-terminal
            ansi-wl-pprint
            async
            base
            base58-bytestring
            base64-bytestring
            binary
            bytestring
            canonical-json
            cardano-crypto
            cardano-report-server
            cardano-sl-core
            cardano-sl-db
            cardano-sl-godtossing
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            cereal
            conduit
            containers
            cpphs
            cryptonite
            cryptonite-openssl
            data-default
            deepseq
            deriving-compat
            digest
            directory
            dlist
            dns
            ed25519
            ekg-core
            ekg-statsd
            ekg-wai
            ether
            exceptions
            file-embed
            filelock
            filepath
            focus
            formatting
            generic-arbitrary
            hashable
            http-client
            http-client-tls
            http-conduit
            http-types
            iproute
            kademlia
            lens
            list-t
            log-warper
            lrucache
            memory
            mmorph
            monad-control
            monad-loops
            MonadRandom
            mono-traversable
            mtl
            neat-interpolation
            network-info
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            plutus-prototype
            pvss
            QuickCheck
            quickcheck-instances
            random
            reflection
            resourcet
            rocksdb-haskell
            safe-exceptions
            safecopy
            serokell-util
            servant
            servant-multipart
            servant-server
            stm
            stm-containers
            string-qq
            systemd
            tagged
            template-haskell
            text
            text-format
            th-lift-instances
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unix
            unordered-containers
            vector
            wai
            wai-extra
            warp
            warp-tls
            yaml
          ];
          executableHaskellDepends = [
            base
            binary
            bytestring
            cardano-sl-core
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            cardano-sl-update
            containers
            data-default
            directory
            ether
            filepath
            formatting
            lens
            log-warper
            mtl
            neat-interpolation
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            serokell-util
            stm-containers
            time
            time-units
            universum
          ];
          testHaskellDepends = [
            aeson
            base
            bytestring
            canonical-json
            cardano-sl-core
            cardano-sl-db
            cardano-sl-godtossing
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            cborg
            cereal
            containers
            cryptonite
            data-default
            ether
            exceptions
            formatting
            generic-arbitrary
            hspec
            kademlia
            lens
            log-warper
            memory
            mmorph
            monad-control
            MonadRandom
            mtl
            network-uri
            node-sketch
            pvss
            QuickCheck
            quickcheck-instances
            random
            reflection
            regex-tdfa
            regex-tdfa-text
            safecopy
            serokell-util
            tagged
            text
            text-format
            time-units
            transformers-base
            universum
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL main implementation";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-auxx = callPackage ({ QuickCheck, acid-state, ansi-wl-pprint, base, base58-bytestring, binary, bytestring, canonical-json, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-godtossing, cardano-sl-infra, cardano-sl-ssc, cardano-sl-txp, cardano-sl-update, containers, cpphs, data-default, dlist, ether, exceptions, formatting, lens, log-warper, mkDerivation, mmorph, monad-control, monad-loops, mtl, neat-interpolation, network-transport-tcp, node-sketch, optparse-applicative, parsec, random, resourcet, safe-exceptions, safecopy, serokell-util, stdenv, stm, stm-containers, tagged, text, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-auxx";
          version = "1.0.3";
          src = ./../auxx;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [
            acid-state
            ansi-wl-pprint
            base
            base58-bytestring
            binary
            bytestring
            canonical-json
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-godtossing
            cardano-sl-infra
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            containers
            data-default
            dlist
            ether
            exceptions
            formatting
            lens
            log-warper
            mmorph
            monad-control
            monad-loops
            mtl
            neat-interpolation
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            QuickCheck
            random
            resourcet
            safe-exceptions
            safecopy
            serokell-util
            stm
            stm-containers
            tagged
            text
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unix
            unordered-containers
          ];
          executableToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - Auxx";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-core = callPackage ({ QuickCheck, aeson, ansi-terminal, autoexporter, base, base16-bytestring, base58-bytestring, binary, bytestring, canonical-json, cardano-crypto, cborg, cereal, concurrent-extra, containers, contravariant, cpphs, cryptonite, cryptonite-openssl, data-default, deepseq, deriving-compat, digest, directory, ed25519, ether, exceptions, file-embed, filepath, formatting, generic-arbitrary, hashable, lens, log-warper, lrucache, memory, mkDerivation, mmorph, mtl, node-sketch, parsec, plutus-prototype, pvss, quickcheck-instances, random, reflection, resourcet, safecopy, scrypt, semigroups, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, th-lift-instances, th-utilities, time, time-units, transformers, transformers-base, transformers-lift, universum, unordered-containers, vector, yaml }:
      mkDerivation {
          pname = "cardano-sl-core";
          version = "1.0.3";
          src = ./../core;
          libraryHaskellDepends = [
            aeson
            ansi-terminal
            autoexporter
            base
            base16-bytestring
            base58-bytestring
            binary
            bytestring
            canonical-json
            cardano-crypto
            cborg
            cereal
            concurrent-extra
            containers
            contravariant
            cryptonite
            cryptonite-openssl
            data-default
            deepseq
            deriving-compat
            digest
            directory
            ed25519
            ether
            exceptions
            file-embed
            filepath
            formatting
            generic-arbitrary
            hashable
            lens
            log-warper
            lrucache
            memory
            mmorph
            mtl
            node-sketch
            parsec
            plutus-prototype
            pvss
            QuickCheck
            quickcheck-instances
            random
            reflection
            resourcet
            safecopy
            scrypt
            semigroups
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
            th-lift-instances
            th-utilities
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unordered-containers
            vector
            yaml
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - core";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-db = callPackage ({ base, bytestring, cardano-sl-core, concurrent-extra, conduit, containers, cpphs, data-default, directory, ether, filepath, formatting, lens, log-warper, mkDerivation, mmorph, monad-control, mtl, node-sketch, resourcet, rocksdb-haskell, serokell-util, stdenv, text-format, transformers, transformers-base, transformers-lift, universum }:
      mkDerivation {
          pname = "cardano-sl-db";
          version = "1.0.3";
          src = ./../db;
          libraryHaskellDepends = [
            base
            bytestring
            cardano-sl-core
            concurrent-extra
            conduit
            containers
            data-default
            directory
            ether
            filepath
            formatting
            lens
            log-warper
            mmorph
            monad-control
            mtl
            node-sketch
            resourcet
            rocksdb-haskell
            serokell-util
            text-format
            transformers
            transformers-base
            transformers-lift
            universum
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - basic DB interfaces";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-explorer = callPackage ({ MonadRandom, QuickCheck, aeson, base, base16-bytestring, binary, bytestring, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-godtossing, cardano-sl-infra, cardano-sl-ssc, cardano-sl-update, cborg, cereal, containers, cpphs, cryptonite, data-default, either, engine-io, engine-io-wai, ether, exceptions, formatting, generic-arbitrary, hspec, http-types, kademlia, lens, lifted-base, log-warper, memory, mkDerivation, mmorph, monad-control, monad-loops, mtl, network-transport-tcp, node-sketch, optparse-applicative, optparse-simple, purescript-bridge, pvss, quickcheck-instances, random, reflection, regex-tdfa, regex-tdfa-text, safecopy, serokell-util, servant, servant-multipart, servant-server, servant-swagger, servant-swagger-ui, socket-io, stdenv, stm, swagger2, tagged, text, text-format, time, time-units, transformers, transformers-base, universum, unordered-containers, vector, wai, wai-cors, wai-extra, warp }:
      mkDerivation {
          pname = "cardano-sl-explorer";
          version = "1.0.3";
          src = ./../explorer;
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            base
            base16-bytestring
            binary
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-godtossing
            cardano-sl-infra
            cardano-sl-ssc
            cardano-sl-update
            containers
            either
            engine-io
            engine-io-wai
            ether
            exceptions
            formatting
            http-types
            lens
            lifted-base
            log-warper
            memory
            monad-control
            monad-loops
            mtl
            node-sketch
            serokell-util
            servant
            servant-server
            socket-io
            stm
            tagged
            text
            text-format
            time
            time-units
            transformers
            universum
            unordered-containers
            wai
            wai-cors
            wai-extra
            warp
          ];
          libraryToolDepends = [ cpphs ];
          executableHaskellDepends = [
            aeson
            base
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-godtossing
            cardano-sl-infra
            cardano-sl-ssc
            cardano-sl-update
            containers
            data-default
            ether
            formatting
            lens
            log-warper
            mtl
            network-transport-tcp
            node-sketch
            optparse-applicative
            optparse-simple
            purescript-bridge
            serokell-util
            servant-multipart
            servant-server
            servant-swagger
            servant-swagger-ui
            swagger2
            text
            time
            time-units
            universum
          ];
          executableToolDepends = [
            cpphs
          ];
          testHaskellDepends = [
            base
            bytestring
            cardano-sl
            cardano-sl-core
            cborg
            cereal
            containers
            cryptonite
            data-default
            ether
            exceptions
            formatting
            generic-arbitrary
            hspec
            kademlia
            lens
            log-warper
            memory
            mmorph
            monad-control
            MonadRandom
            mtl
            node-sketch
            pvss
            QuickCheck
            quickcheck-instances
            random
            reflection
            regex-tdfa
            regex-tdfa-text
            safecopy
            serokell-util
            tagged
            text-format
            time-units
            transformers-base
            universum
            unordered-containers
            vector
          ];
          testToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano explorer";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-godtossing = callPackage ({ QuickCheck, aeson, array, base, bytestring, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, containers, cpphs, cryptonite, data-default, directory, ether, file-embed, filepath, formatting, generic-arbitrary, hashable, lens, log-warper, mkDerivation, mmorph, mono-traversable, mtl, node-sketch, random, reflection, rocksdb-haskell, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, time-units, transformers, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-godtossing";
          version = "1.0.3";
          src = ./../godtossing;
          libraryHaskellDepends = [
            aeson
            array
            base
            bytestring
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            containers
            cryptonite
            data-default
            directory
            ether
            file-embed
            filepath
            formatting
            generic-arbitrary
            hashable
            lens
            log-warper
            mmorph
            mono-traversable
            mtl
            node-sketch
            QuickCheck
            random
            reflection
            rocksdb-haskell
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
            time-units
            transformers
            universum
            unordered-containers
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - GodTossing implementation of SSC";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-infra = callPackage ({ QuickCheck, aeson, base, base64-bytestring, binary, bytestring, cardano-report-server, cardano-sl-core, cardano-sl-db, containers, cpphs, data-default, directory, dns, either, ekg-core, ether, exceptions, filepath, formatting, generic-arbitrary, hashable, http-client, http-client-tls, iproute, kademlia, lens, list-t, log-warper, mkDerivation, mmorph, monad-control, mtl, network-info, network-transport, network-transport-tcp, node-sketch, optparse-applicative, parsec, reflection, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers, yaml }:
      mkDerivation {
          pname = "cardano-sl-infra";
          version = "1.0.3";
          src = ./../infra;
          libraryHaskellDepends = [
            aeson
            base
            base64-bytestring
            binary
            bytestring
            cardano-report-server
            cardano-sl-core
            cardano-sl-db
            containers
            data-default
            directory
            dns
            either
            ekg-core
            ether
            exceptions
            filepath
            formatting
            generic-arbitrary
            hashable
            http-client
            http-client-tls
            iproute
            kademlia
            lens
            list-t
            log-warper
            mmorph
            monad-control
            mtl
            network-info
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            QuickCheck
            reflection
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unix
            unordered-containers
            yaml
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - infrastructural";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-lrc = callPackage ({ QuickCheck, base, bytestring, cardano-sl-core, cardano-sl-db, conduit, cpphs, ether, formatting, generic-arbitrary, lens, log-warper, mkDerivation, node-sketch, reflection, rocksdb-haskell, stdenv, text-format, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-lrc";
          version = "1.0.3";
          src = ./../lrc;
          libraryHaskellDepends = [
            base
            bytestring
            cardano-sl-core
            cardano-sl-db
            conduit
            ether
            formatting
            generic-arbitrary
            lens
            log-warper
            node-sketch
            QuickCheck
            reflection
            rocksdb-haskell
            text-format
            universum
            unordered-containers
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - Leaders and Richmen computation";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-ssc = callPackage ({ QuickCheck, aeson, base, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cpphs, cryptonite, data-default, ether, exceptions, formatting, lens, log-warper, memory, mkDerivation, mmorph, mtl, node-sketch, parsec, serokell-util, stdenv, stm, tagged, text-format, universum }:
      mkDerivation {
          pname = "cardano-sl-ssc";
          version = "1.0.3";
          src = ./../ssc;
          libraryHaskellDepends = [
            aeson
            base
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            cryptonite
            data-default
            ether
            exceptions
            formatting
            lens
            log-warper
            memory
            mmorph
            mtl
            node-sketch
            parsec
            QuickCheck
            serokell-util
            stm
            tagged
            text-format
            universum
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - the SSC class";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-tools = callPackage ({ Chart, Chart-diagrams, Glob, MonadRandom, QuickCheck, aeson, ansi-wl-pprint, array, async, attoparsec, base, base58-bytestring, bytestring, canonical-json, cardano-report-server, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, cardano-sl-txp, containers, cpphs, cryptonite, data-default, directory, ed25519, ether, fgl, filepath, foldl, formatting, graphviz, kademlia, lens, lifted-async, log-warper, mkDerivation, mtl, neat-interpolation, node-sketch, optparse-applicative, parsec, pipes, pipes-bytestring, pipes-interleave, pipes-safe, process, random, random-shuffle, safe-exceptions, serokell-util, silently, stdenv, stm, system-filepath, tar, text, text-format, time, time-units, universum, unix, unix-compat, unordered-containers, vector, yaml }:
      mkDerivation {
          pname = "cardano-sl-tools";
          version = "1.0.3";
          src = ./../tools;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [
            aeson
            ansi-wl-pprint
            array
            async
            attoparsec
            base
            base58-bytestring
            bytestring
            canonical-json
            cardano-report-server
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            cardano-sl-txp
            Chart
            Chart-diagrams
            containers
            cryptonite
            data-default
            directory
            ed25519
            ether
            fgl
            filepath
            foldl
            formatting
            Glob
            graphviz
            kademlia
            lens
            lifted-async
            log-warper
            MonadRandom
            mtl
            neat-interpolation
            node-sketch
            optparse-applicative
            parsec
            pipes
            pipes-bytestring
            pipes-interleave
            pipes-safe
            process
            QuickCheck
            random
            random-shuffle
            safe-exceptions
            serokell-util
            silently
            stm
            system-filepath
            tar
            text
            text-format
            time
            time-units
            universum
            unix
            unix-compat
            unordered-containers
            vector
            yaml
          ];
          executableToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - Tools";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-txp = callPackage ({ QuickCheck, aeson, base, bytestring, cardano-sl-core, cardano-sl-db, cardano-sl-infra, conduit, containers, cpphs, data-default, ekg-core, ether, exceptions, formatting, generic-arbitrary, hashable, lens, lifted-base, log-warper, memory, mkDerivation, monad-control, mtl, neat-interpolation, node-sketch, plutus-prototype, resourcet, rocksdb-haskell, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, time-units, transformers, universum, unordered-containers, vector }:
      mkDerivation {
          pname = "cardano-sl-txp";
          version = "1.0.3";
          src = ./../txp;
          libraryHaskellDepends = [
            aeson
            base
            bytestring
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            conduit
            containers
            data-default
            ekg-core
            ether
            exceptions
            formatting
            generic-arbitrary
            hashable
            lens
            lifted-base
            log-warper
            memory
            monad-control
            mtl
            neat-interpolation
            node-sketch
            plutus-prototype
            QuickCheck
            resourcet
            rocksdb-haskell
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
            time-units
            transformers
            universum
            unordered-containers
            vector
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - transaction processing";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-update = callPackage ({ QuickCheck, aeson, base, binary, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, concurrent-extra, conduit, containers, cpphs, data-default, ether, exceptions, formatting, generic-arbitrary, hashable, lens, log-warper, mkDerivation, mtl, node-sketch, parsec, reflection, resourcet, rocksdb-haskell, safecopy, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, th-lift-instances, time-units, transformers, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-update";
          version = "1.0.3";
          src = ./../update;
          libraryHaskellDepends = [
            aeson
            base
            binary
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            concurrent-extra
            conduit
            containers
            data-default
            ether
            exceptions
            formatting
            generic-arbitrary
            hashable
            lens
            log-warper
            mtl
            node-sketch
            parsec
            QuickCheck
            reflection
            resourcet
            rocksdb-haskell
            safecopy
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
            th-lift-instances
            time-units
            transformers
            universum
            unordered-containers
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - update";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-wallet = callPackage ({ acid-state, aeson, ansi-wl-pprint, base, base58-bytestring, binary, bytestring, cardano-report-server, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-godtossing, cardano-sl-infra, cardano-sl-ssc, cardano-sl-txp, cardano-sl-update, containers, cpphs, data-default, directory, dlist, ether, exceptions, filepath, formatting, lens, log-warper, mkDerivation, mtl, network-transport, network-transport-tcp, node-sketch, optparse-applicative, parsec, purescript-bridge, random, reflection, safe-exceptions, semver, serokell-util, servant, servant-multipart, servant-server, servant-swagger, servant-swagger-ui, stdenv, stm, stm-containers, string-qq, swagger2, text, text-format, time, time-units, transformers, universum, unix, unordered-containers, wai, wai-websockets, websockets }:
      mkDerivation {
          pname = "cardano-sl-wallet";
          version = "1.0.3";
          src = ./../wallet;
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            ansi-wl-pprint
            base
            base58-bytestring
            binary
            bytestring
            cardano-report-server
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-godtossing
            cardano-sl-infra
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            containers
            data-default
            directory
            dlist
            ether
            exceptions
            filepath
            formatting
            lens
            log-warper
            mtl
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            random
            reflection
            semver
            serokell-util
            servant
            servant-multipart
            servant-server
            servant-swagger
            servant-swagger-ui
            stm
            stm-containers
            string-qq
            swagger2
            text
            text-format
            time
            time-units
            transformers
            universum
            unix
            unordered-containers
            wai
            wai-websockets
            websockets
          ];
          libraryToolDepends = [ cpphs ];
          executableHaskellDepends = [
            acid-state
            aeson
            ansi-wl-pprint
            base
            base58-bytestring
            binary
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-godtossing
            cardano-sl-infra
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            containers
            data-default
            directory
            dlist
            ether
            exceptions
            filepath
            formatting
            lens
            log-warper
            mtl
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            purescript-bridge
            random
            safe-exceptions
            serokell-util
            servant
            servant-multipart
            servant-server
            servant-swagger
            servant-swagger-ui
            stm
            stm-containers
            string-qq
            swagger2
            text
            text-format
            time
            time-units
            transformers
            universum
            unordered-containers
            wai
            wai-websockets
            websockets
          ];
          executableToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - wallet";
          license = stdenv.lib.licenses.mit;
        }) {};
      case-insensitive = callPackage ({ base, bytestring, deepseq, hashable, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "case-insensitive";
          version = "1.2.0.10";
          sha256 = "66321c40fffb35f3a3188ba508753b74aada53fb51c822a9752614b03765306c";
          libraryHaskellDepends = [
            base
            bytestring
            deepseq
            hashable
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/case-insensitive";
          description = "Case insensitive string comparison";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cborg = callPackage ({ array, base, bytestring, containers, ghc-prim, half, integer-gmp, mkDerivation, primitive, stdenv, text }:
      mkDerivation {
          pname = "cborg";
          version = "0.1.1.0";
          sha256 = "f23a477ffb22778efa5dbf0230ae68272d2dc0593c594d6d22f4975079961488";
          revision = "1";
          editedCabalFile = "0qqg1gfjf869ynrh20fbrpfhjf2yh6v3i5s6w327sirbhw9ajk6v";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
            ghc-prim
            half
            integer-gmp
            primitive
            text
          ];
          doHaddock = false;
          doCheck = false;
          description = "Concise Binary Object Representation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cereal = callPackage ({ array, base, bytestring, containers, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "cereal";
          version = "0.5.4.0";
          sha256 = "daca6c5aeff21ca233bebe006c158b0e4421b239c722768b568fca9b32cafee7";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/GaloisInc/cereal";
          description = "A binary serialization library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cereal-vector = callPackage ({ base, bytestring, cereal, mkDerivation, stdenv, vector }:
      mkDerivation {
          pname = "cereal-vector";
          version = "0.2.0.1";
          sha256 = "ff0685a6c39e7aae32f8b4165e2ae06f284c867298ad4f7b776c1c1b2859f933";
          libraryHaskellDepends = [
            base
            bytestring
            cereal
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/acfoltzer/cereal-vector";
          description = "Serialize instances for Data.Vector types.";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      clock = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "clock";
          version = "0.7.2";
          sha256 = "886601978898d3a91412fef895e864576a7125d661e1f8abc49a2a08840e691f";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/corsis/clock";
          description = "High-resolution clock functions: monotonic, realtime, cputime";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cmdargs = callPackage ({ base, filepath, mkDerivation, process, stdenv, template-haskell, transformers }:
      mkDerivation {
          pname = "cmdargs";
          version = "0.10.17";
          sha256 = "3437a4caf4ced650b61620e1c66f406db76ff70244928e5a4e7a20e5e88374da";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            filepath
            process
            template-haskell
            transformers
          ];
          executableHaskellDepends = [
            base
            filepath
            process
            template-haskell
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ndmitchell/cmdargs#readme";
          description = "Command line argument processing";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      colour = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "colour";
          version = "2.3.3";
          sha256 = "8d15a63494f8e2a06fe6dc38baee8e948adfae0e93749b9e3ce0fd8ece09b6e2";
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.haskell.org/haskellwiki/Colour";
          description = "A model for human colour/color perception";
          license = stdenv.lib.licenses.mit;
        }) {};
      comonad = callPackage ({ Cabal, base, cabal-doctest, containers, contravariant, distributive, mkDerivation, semigroups, stdenv, tagged, transformers, transformers-compat }:
      mkDerivation {
          pname = "comonad";
          version = "5.0.2";
          sha256 = "1bb0fe396ecd16008411862ee453e8bd7c3e0f3a7299537dd59466604a54b784";
          revision = "1";
          editedCabalFile = "1lnsnx8p3wlfhd1xfc68za3b00vq77z2m6b0vqiw2laqmpj9akcw";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            base
            containers
            contravariant
            distributive
            semigroups
            tagged
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/comonad/";
          description = "Comonads";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      concurrent-extra = callPackage ({ base, mkDerivation, stdenv, stm, unbounded-delays }:
      mkDerivation {
          pname = "concurrent-extra";
          version = "0.7.0.10";
          sha256 = "6f27cc0a90f5f25b3c0a1e9e3c0e3b407538908c061c5b7da34461b76e1adc12";
          libraryHaskellDepends = [
            base
            stm
            unbounded-delays
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/concurrent-extra";
          description = "Extra concurrency primitives";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      conduit = callPackage ({ base, exceptions, lifted-base, mkDerivation, mmorph, monad-control, mtl, primitive, resourcet, stdenv, transformers, transformers-base }:
      mkDerivation {
          pname = "conduit";
          version = "1.2.11";
          sha256 = "0b66423f04d991262b800174064d0c6046fba0009eddcca616f9afaf84dca8f7";
          libraryHaskellDepends = [
            base
            exceptions
            lifted-base
            mmorph
            monad-control
            mtl
            primitive
            resourcet
            transformers
            transformers-base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/snoyberg/conduit";
          description = "Streaming data processing library";
          license = stdenv.lib.licenses.mit;
        }) {};
      conduit-extra = callPackage ({ async, attoparsec, base, blaze-builder, bytestring, conduit, directory, exceptions, filepath, mkDerivation, monad-control, network, primitive, process, resourcet, stdenv, stm, streaming-commons, text, transformers, transformers-base }:
      mkDerivation {
          pname = "conduit-extra";
          version = "1.1.16";
          sha256 = "bd72c1bacd5f59a74a73a0aa115b8314f0a1dc1b24d939e52a983113c960f8d5";
          libraryHaskellDepends = [
            async
            attoparsec
            base
            blaze-builder
            bytestring
            conduit
            directory
            exceptions
            filepath
            monad-control
            network
            primitive
            process
            resourcet
            stm
            streaming-commons
            text
            transformers
            transformers-base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/snoyberg/conduit";
          description = "Batteries included conduit: adapters for common libraries";
          license = stdenv.lib.licenses.mit;
        }) {};
      connection = callPackage ({ base, byteable, bytestring, containers, data-default-class, mkDerivation, network, socks, stdenv, tls, x509, x509-store, x509-system, x509-validation }:
      mkDerivation {
          pname = "connection";
          version = "0.2.8";
          sha256 = "70b1f44e8786320c18b26fc5d4ec115fc8ac016ba1f852fa8137f55d785a93eb";
          libraryHaskellDepends = [
            base
            byteable
            bytestring
            containers
            data-default-class
            network
            socks
            tls
            x509
            x509-store
            x509-system
            x509-validation
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-connection";
          description = "Simple and easy network connections API";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      constraints = callPackage ({ base, binary, deepseq, ghc-prim, hashable, mkDerivation, mtl, stdenv, transformers, transformers-compat }:
      mkDerivation {
          pname = "constraints";
          version = "0.9.1";
          sha256 = "276e012838861145fca65d065dd9839f7cbd71236032b557194389180a30a785";
          libraryHaskellDepends = [
            base
            binary
            deepseq
            ghc-prim
            hashable
            mtl
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/constraints/";
          description = "Constraint manipulation";
          license = stdenv.lib.licenses.bsd2;
        }) {};
      containers = callPackage ({ array, base, deepseq, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "containers";
          version = "0.5.7.1";
          sha256 = "73856c3307e2ea26c33474309af4dcdfb80e7644e9a82ef4146c742a6e400f79";
          libraryHaskellDepends = [
            array
            base
            deepseq
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          description = "Assorted concrete container types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      contravariant = callPackage ({ StateVar, base, mkDerivation, semigroups, stdenv, transformers, transformers-compat, void }:
      mkDerivation {
          pname = "contravariant";
          version = "1.4";
          sha256 = "e1666df1373ed784baa7d1e8e963bbc2d1f3c391578ac550ae74e7399173ee84";
          libraryHaskellDepends = [
            base
            semigroups
            StateVar
            transformers
            transformers-compat
            void
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/contravariant/";
          description = "Contravariant functors";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cookie = callPackage ({ base, blaze-builder, bytestring, data-default-class, deepseq, mkDerivation, old-locale, stdenv, text, time }:
      mkDerivation {
          pname = "cookie";
          version = "0.4.2.1";
          sha256 = "06413091908e20ce154effdcd354d7eea1447380e29a8acdb15c3347512852e4";
          libraryHaskellDepends = [
            base
            blaze-builder
            bytestring
            data-default-class
            deepseq
            old-locale
            text
            time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/snoyberg/cookie";
          description = "HTTP cookie parsing and rendering";
          license = stdenv.lib.licenses.mit;
        }) {};
      cpphs = callPackage ({ base, directory, mkDerivation, old-locale, old-time, polyparse, stdenv }:
      mkDerivation {
          pname = "cpphs";
          version = "1.20.8";
          sha256 = "e56d64a7d8058e0fb63f0669397c1c861efb20a0376e0e74d86942ac151105ae";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            directory
            old-locale
            old-time
            polyparse
          ];
          executableHaskellDepends = [
            base
            directory
            old-locale
            old-time
            polyparse
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://projects.haskell.org/cpphs/";
          description = "A liberalised re-implementation of cpp, the C pre-processor";
          license = "LGPL";
        }) {};
      cryptohash-md5 = callPackage ({ base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "cryptohash-md5";
          version = "0.11.100.1";
          sha256 = "710bd48770fa3e9a3b05428c6dc77fb72c91956d334a1eb89ded11bb843e18f9";
          revision = "1";
          editedCabalFile = "1drxjsn5chi9zj3djj85s1d6xqlc28ji70zpyicxl5fals10n5w3";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hvr/cryptohash-md5";
          description = "Fast, pure and practical MD5 implementation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cryptohash-sha1 = callPackage ({ base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "cryptohash-sha1";
          version = "0.11.100.1";
          sha256 = "3c79af33542512442f8f87f6abb1faef7cd43bbfb2859260a33251d861eb0dab";
          revision = "1";
          editedCabalFile = "167i2mjyr18949xckzv6f782n763f6w9k114p6kq74gbmxqjvmqb";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hvr/cryptohash-sha1";
          description = "Fast, pure and practical SHA-1 implementation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cryptonite = callPackage ({ base, bytestring, deepseq, foundation, ghc-prim, integer-gmp, memory, mkDerivation, stdenv }:
      mkDerivation {
          pname = "cryptonite";
          version = "0.23";
          sha256 = "ee4a1c2cec13f3697a2a35255022fe802b2e29cd836b280702f2495b5f6f0099";
          libraryHaskellDepends = [
            base
            bytestring
            deepseq
            foundation
            ghc-prim
            integer-gmp
            memory
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-crypto/cryptonite";
          description = "Cryptography Primitives sink";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cryptonite-openssl = callPackage ({ base, bytestring, cryptonite, memory, mkDerivation, openssl, stdenv }:
      mkDerivation {
          pname = "cryptonite-openssl";
          version = "0.6";
          sha256 = "a8cb97c96bfb3e7b7ff8d59629317882dbf3cea12ba978d8475c96a6c28750a6";
          libraryHaskellDepends = [
            base
            bytestring
            cryptonite
            memory
          ];
          librarySystemDepends = [
            openssl
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-crypto/cryptonite-openssl";
          description = "Crypto stuff using OpenSSL cryptographic library";
          license = stdenv.lib.licenses.bsd3;
        }) { openssl = pkgs.openssl; };
      data-accessor = callPackage ({ array, base, containers, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "data-accessor";
          version = "0.2.2.7";
          sha256 = "3465227ad5f81059a885d354e2f3c108d550287580e6939e18350fa65e78c2ed";
          libraryHaskellDepends = [
            array
            base
            containers
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.haskell.org/haskellwiki/Record_access";
          description = "Utilities for accessing and manipulating fields of records";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      data-default = callPackage ({ base, data-default-class, data-default-instances-containers, data-default-instances-dlist, data-default-instances-old-locale, mkDerivation, stdenv }:
      mkDerivation {
          pname = "data-default";
          version = "0.7.1.1";
          sha256 = "b0f95d279cd75cacaa8152a01590dc3460f7134f6840b37052abb3ba3cb2a511";
          libraryHaskellDepends = [
            base
            data-default-class
            data-default-instances-containers
            data-default-instances-dlist
            data-default-instances-old-locale
          ];
          doHaddock = false;
          doCheck = false;
          description = "A class for types with a default value";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      data-default-class = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "data-default-class";
          version = "0.1.2.0";
          sha256 = "4f01b423f000c3e069aaf52a348564a6536797f31498bb85c3db4bd2d0973e56";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "A class for types with a default value";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      data-default-instances-containers = callPackage ({ base, containers, data-default-class, mkDerivation, stdenv }:
      mkDerivation {
          pname = "data-default-instances-containers";
          version = "0.0.1";
          sha256 = "a55e07af005c9815d82f3fc95e125db82994377c9f4a769428878701d4ec081a";
          libraryHaskellDepends = [
            base
            containers
            data-default-class
          ];
          doHaddock = false;
          doCheck = false;
          description = "Default instances for types in containers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      data-default-instances-dlist = callPackage ({ base, data-default-class, dlist, mkDerivation, stdenv }:
      mkDerivation {
          pname = "data-default-instances-dlist";
          version = "0.0.1";
          sha256 = "7d683711cbf08abd7adcd5ac2be825381308d220397315a5570fe61b719b5959";
          libraryHaskellDepends = [
            base
            data-default-class
            dlist
          ];
          doHaddock = false;
          doCheck = false;
          description = "Default instances for types in dlist";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      data-default-instances-old-locale = callPackage ({ base, data-default-class, mkDerivation, old-locale, stdenv }:
      mkDerivation {
          pname = "data-default-instances-old-locale";
          version = "0.0.1";
          sha256 = "60d3b02922958c4908d7bf2b24ddf61511665745f784227d206745784b0c0802";
          libraryHaskellDepends = [
            base
            data-default-class
            old-locale
          ];
          doHaddock = false;
          doCheck = false;
          description = "Default instances for types in old-locale";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      deepseq = callPackage ({ array, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "deepseq";
          version = "1.4.2.0";
          sha256 = "de0aa1291790409fe36e8b9bdf3c1f340661290eb3258876af2b07b721e94951";
          libraryHaskellDepends = [
            array
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Deep evaluation of data structures";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      deriving-compat = callPackage ({ base, containers, ghc-boot-th, ghc-prim, mkDerivation, stdenv, template-haskell, transformers, transformers-compat }:
      mkDerivation {
          pname = "deriving-compat";
          version = "0.3.6";
          sha256 = "0c1fab416505e3fabaec007828073c065db077f004dcc6955f2cd32ca139356d";
          libraryHaskellDepends = [
            base
            containers
            ghc-boot-th
            ghc-prim
            template-haskell
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-compat/deriving-compat";
          description = "Backports of GHC deriving extensions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      diagrams-core = callPackage ({ adjunctions, base, containers, distributive, dual-tree, lens, linear, mkDerivation, monoid-extras, mtl, profunctors, semigroups, stdenv, unordered-containers }:
      mkDerivation {
          pname = "diagrams-core";
          version = "1.4";
          sha256 = "e5502f483dadb86056523d601a1037596ff49380b4c1cd00600183eab7992ae7";
          libraryHaskellDepends = [
            adjunctions
            base
            containers
            distributive
            dual-tree
            lens
            linear
            monoid-extras
            mtl
            profunctors
            semigroups
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://projects.haskell.org/diagrams";
          description = "Core libraries for diagrams EDSL";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      diagrams-lib = callPackage ({ JuicyPixels, active, adjunctions, array, base, bytestring, cereal, colour, containers, data-default-class, diagrams-core, diagrams-solve, directory, distributive, dual-tree, exceptions, filepath, fingertree, fsnotify, hashable, intervals, lens, linear, mkDerivation, monoid-extras, mtl, optparse-applicative, process, profunctors, semigroups, stdenv, tagged, text, transformers, unordered-containers }:
      mkDerivation {
          pname = "diagrams-lib";
          version = "1.4.1.2";
          sha256 = "4b6e8805decaef85d355d620311595b16fb702df3885060db19bc9b425652670";
          revision = "4";
          editedCabalFile = "0wlb4ng803rhx82msl49b39im4cw8naik0pcyyybpphyqbxxs6dd";
          libraryHaskellDepends = [
            active
            adjunctions
            array
            base
            bytestring
            cereal
            colour
            containers
            data-default-class
            diagrams-core
            diagrams-solve
            directory
            distributive
            dual-tree
            exceptions
            filepath
            fingertree
            fsnotify
            hashable
            intervals
            JuicyPixels
            lens
            linear
            monoid-extras
            mtl
            optparse-applicative
            process
            profunctors
            semigroups
            tagged
            text
            transformers
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://projects.haskell.org/diagrams";
          description = "Embedded domain-specific language for declarative graphics";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      diagrams-postscript = callPackage ({ base, containers, data-default-class, diagrams-core, diagrams-lib, dlist, filepath, hashable, lens, mkDerivation, monoid-extras, mtl, semigroups, split, statestack, stdenv }:
      mkDerivation {
          pname = "diagrams-postscript";
          version = "1.4";
          sha256 = "fe58f0010520716f66802adb0c1f70f48e77e9c4fcea5441e5343f4c1a5f8db4";
          revision = "1";
          editedCabalFile = "0vmiv3b74nml0ahb7dicq0m0vz2lahzfapln9aby0jb2saa0sf58";
          libraryHaskellDepends = [
            base
            containers
            data-default-class
            diagrams-core
            diagrams-lib
            dlist
            filepath
            hashable
            lens
            monoid-extras
            mtl
            semigroups
            split
            statestack
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://projects.haskell.org/diagrams/";
          description = "Postscript backend for diagrams drawing EDSL";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      diagrams-solve = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "diagrams-solve";
          version = "0.1.1";
          sha256 = "a41f5f410b10f162b1e5c07bd4ca3305544870ff1314ae4f5824c83a31644f9d";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://projects.haskell.org/diagrams";
          description = "Pure Haskell solver routines used by diagrams";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      diagrams-svg = callPackage ({ JuicyPixels, base, base64-bytestring, bytestring, colour, containers, diagrams-core, diagrams-lib, filepath, hashable, lens, mkDerivation, monoid-extras, mtl, optparse-applicative, semigroups, split, stdenv, svg-builder, text }:
      mkDerivation {
          pname = "diagrams-svg";
          version = "1.4.1";
          sha256 = "ce691378025835c7e794898a5f03299341f5f1e35a20de4afd12b1f9b0667f87";
          revision = "1";
          editedCabalFile = "12cp0898pplap5skhq43xsxh0m2ilv5lz9zw2fhkkjmnr4pbl2dx";
          libraryHaskellDepends = [
            base
            base64-bytestring
            bytestring
            colour
            containers
            diagrams-core
            diagrams-lib
            filepath
            hashable
            JuicyPixels
            lens
            monoid-extras
            mtl
            optparse-applicative
            semigroups
            split
            svg-builder
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://projects.haskell.org/diagrams/";
          description = "SVG backend for diagrams drawing EDSL";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      digest = callPackage ({ base, bytestring, mkDerivation, stdenv, zlib }:
      mkDerivation {
          pname = "digest";
          version = "0.0.1.2";
          sha256 = "641717eb16392abf8965986a9e8dc21eebf1d97775bbb6923c7b7f8fee17fe11";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          librarySystemDepends = [ zlib ];
          doHaddock = false;
          doCheck = false;
          description = "Various cryptographic hashes for bytestrings; CRC32 and Adler32 for now";
          license = stdenv.lib.licenses.bsd3;
        }) { zlib = pkgs.zlib; };
      directory = callPackage ({ base, filepath, mkDerivation, stdenv, time, unix }:
      mkDerivation {
          pname = "directory";
          version = "1.3.1.0";
          sha256 = "94b0d06aba8311e3b9dc8e460d4ad5b25fdfcc361eecb8e7ad68a18f171aa7f2";
          libraryHaskellDepends = [
            base
            filepath
            time
            unix
          ];
          doHaddock = false;
          doCheck = false;
          description = "Platform-agnostic library for filesystem operations";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      distributive = callPackage ({ Cabal, base, base-orphans, cabal-doctest, mkDerivation, stdenv, tagged, transformers, transformers-compat }:
      mkDerivation {
          pname = "distributive";
          version = "0.5.3";
          sha256 = "9173805b9c941bda1f37e5aeb68ae30f57a12df9b17bd2aa86db3b7d5236a678";
          revision = "2";
          editedCabalFile = "02j27xvlj0jw3b2jpfg6wbysj0blllin792wj6qnrgnrvd4haj7v";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            base
            base-orphans
            tagged
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/distributive/";
          description = "Distributive functors -- Dual to Traversable";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      dlist = callPackage ({ base, deepseq, mkDerivation, stdenv }:
      mkDerivation {
          pname = "dlist";
          version = "0.8.0.3";
          sha256 = "876782c96957ff480863effb33878f48dd55de7fa64d036e12bf1fbd49542f2f";
          libraryHaskellDepends = [
            base
            deepseq
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/spl/dlist";
          description = "Difference lists";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      dns = callPackage ({ attoparsec, base, base64-bytestring, binary, bytestring, bytestring-builder, conduit, conduit-extra, containers, fetchgit, iproute, mkDerivation, mtl, network, random, resourcet, safe, stdenv, text }:
      mkDerivation {
          pname = "dns";
          version = "3.0.0";
          src = fetchgit {
            url = "https://github.com/kazu-yamamoto/dns.git";
            sha256 = "1scbzbbykypcnjp9n5pdvlddgijijz834hwq49p4swvg9p1gypv2";
            rev = "08df7fe6a9242f7d76aa5040221702e26ab610f6";
          };
          libraryHaskellDepends = [
            attoparsec
            base
            base64-bytestring
            binary
            bytestring
            bytestring-builder
            conduit
            conduit-extra
            containers
            iproute
            mtl
            network
            random
            resourcet
            safe
            text
          ];
          doHaddock = false;
          doCheck = false;
          testTarget = "spec";
          description = "DNS library in Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      double-conversion = callPackage ({ base, bytestring, ghc-prim, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "double-conversion";
          version = "2.0.2.0";
          sha256 = "44cde172395401169e844d6791b6eb0ef2c2e55a08de8dda96551cfe029ba26b";
          libraryHaskellDepends = [
            base
            bytestring
            ghc-prim
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/double-conversion";
          description = "Fast conversion between double precision floating point and text";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      dual-tree = callPackage ({ base, mkDerivation, monoid-extras, newtype-generics, semigroups, stdenv }:
      mkDerivation {
          pname = "dual-tree";
          version = "0.2.1";
          sha256 = "2465247dab91c799a06feccc3598c4c25a15bb17e80da102e22a45caa9605f19";
          libraryHaskellDepends = [
            base
            monoid-extras
            newtype-generics
            semigroups
          ];
          doHaddock = false;
          doCheck = false;
          description = "Rose trees with cached and accumulating monoidal annotations";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      easy-file = callPackage ({ base, directory, filepath, mkDerivation, stdenv, time, unix }:
      mkDerivation {
          pname = "easy-file";
          version = "0.2.1";
          sha256 = "ff86e1b29284499bea5f1d0ff539b3ed64fa6d1a06c2243ca61f93be0202e56c";
          libraryHaskellDepends = [
            base
            directory
            filepath
            time
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/kazu-yamamoto/easy-file";
          description = "Cross-platform File handling";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ed25519 = callPackage ({ base, bytestring, fetchgit, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "ed25519";
          version = "0.0.5.0";
          src = fetchgit {
            url = "https://github.com/thoughtpolice/hs-ed25519";
            sha256 = "0fah4vkmqdkjsdh3s3x27yfaif2fbdg6049xvp54b5mh50yvxkfq";
            rev = "da4247b5b3420120e20451e6a252e2a2ca15b43c";
          };
          libraryHaskellDepends = [
            base
            bytestring
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://thoughtpolice.github.com/hs-ed25519";
          description = "Ed25519 cryptographic signatures";
          license = stdenv.lib.licenses.mit;
        }) {};
      either = callPackage ({ MonadRandom, base, bifunctors, exceptions, free, mkDerivation, mmorph, monad-control, mtl, profunctors, semigroupoids, semigroups, stdenv, transformers, transformers-base }:
      mkDerivation {
          pname = "either";
          version = "4.4.1.1";
          sha256 = "b087cb0fb63fec2fbdcac05fef0d03751daef5deb86cda3c732b9a6a31e634d3";
          revision = "2";
          editedCabalFile = "1n7792mcrvfh31qrbj8mpnx372s03kz83mypj7l4fm5h6zi4a3hs";
          libraryHaskellDepends = [
            base
            bifunctors
            exceptions
            free
            mmorph
            monad-control
            MonadRandom
            mtl
            profunctors
            semigroupoids
            semigroups
            transformers
            transformers-base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/either/";
          description = "An either monad transformer";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ekg-core = callPackage ({ base, containers, ghc-prim, mkDerivation, stdenv, text, unordered-containers }:
      mkDerivation {
          pname = "ekg-core";
          version = "0.1.1.3";
          sha256 = "ac56e2d0f6bf0b76aa3b69beddbb7d0811e8991c98a379bc24ec808049fb89e3";
          libraryHaskellDepends = [
            base
            containers
            ghc-prim
            text
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/tibbe/ekg-core";
          description = "Tracking of system metrics";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ekg-json = callPackage ({ aeson, base, ekg-core, mkDerivation, stdenv, text, unordered-containers }:
      mkDerivation {
          pname = "ekg-json";
          version = "0.1.0.6";
          sha256 = "1e6a80aa0a28bbf41c9c6364cbb5731160d14fa54145f27a82d0b3467a04dd47";
          libraryHaskellDepends = [
            aeson
            base
            ekg-core
            text
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/tibbe/ekg-json";
          description = "JSON encoding of ekg metrics";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ekg-statsd = callPackage ({ base, bytestring, ekg-core, mkDerivation, network, stdenv, text, time, unordered-containers }:
      mkDerivation {
          pname = "ekg-statsd";
          version = "0.2.2.0";
          sha256 = "c2a0f4270e2e1daa2847944c8b3bf948df8c6efd4893063b069857fa7e893afc";
          libraryHaskellDepends = [
            base
            bytestring
            ekg-core
            network
            text
            time
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/tibbe/ekg-statsd";
          description = "Push metrics to statsd";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ekg-wai = callPackage ({ aeson, base, bytestring, ekg-core, ekg-json, filepath, http-types, mkDerivation, network, stdenv, text, time, transformers, unordered-containers, wai, wai-app-static, warp }:
      mkDerivation {
          pname = "ekg-wai";
          version = "0.1.0.2";
          sha256 = "dc42eb0c0c7be06595382dc2858cc926825fff87ab617aa47cc8513092652de6";
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            aeson
            base
            bytestring
            ekg-core
            ekg-json
            filepath
            http-types
            network
            text
            time
            transformers
            unordered-containers
            wai
            wai-app-static
            warp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/tvh/ekg-wai";
          description = "Remote monitoring of processes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      engine-io = callPackage ({ aeson, async, attoparsec, base, base64-bytestring, bytestring, either, fetchgit, free, mkDerivation, monad-loops, mwc-random, stdenv, stm, stm-delay, text, transformers, unordered-containers, vector, websockets }:
      mkDerivation {
          pname = "engine-io";
          version = "1.2.15";
          src = fetchgit {
            url = "https://github.com/serokell/engine.io.git";
            sha256 = "0j2rxbw5g88ivmjzhmhnxk4cgkxdw97i2qlzw47gzyv56ciqfdny";
            rev = "a594e402fd450f11ad60d09ddbd93db500000632";
          };
          postUnpack = "sourceRoot+=/engine-io; echo source root reset to \$sourceRoot";
          libraryHaskellDepends = [
            aeson
            async
            attoparsec
            base
            base64-bytestring
            bytestring
            either
            free
            monad-loops
            mwc-random
            stm
            stm-delay
            text
            transformers
            unordered-containers
            vector
            websockets
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ocharles/engine.io";
          description = "A Haskell implementation of Engine.IO";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      engine-io-wai = callPackage ({ attoparsec, base, bytestring, either, engine-io, fetchgit, http-types, mkDerivation, mtl, stdenv, text, transformers, transformers-compat, unordered-containers, wai, wai-websockets, websockets }:
      mkDerivation {
          pname = "engine-io-wai";
          version = "1.0.6";
          src = fetchgit {
            url = "https://github.com/serokell/engine.io.git";
            sha256 = "0j2rxbw5g88ivmjzhmhnxk4cgkxdw97i2qlzw47gzyv56ciqfdny";
            rev = "a594e402fd450f11ad60d09ddbd93db500000632";
          };
          postUnpack = "sourceRoot+=/engine-io-wai; echo source root reset to \$sourceRoot";
          libraryHaskellDepends = [
            attoparsec
            base
            bytestring
            either
            engine-io
            http-types
            mtl
            text
            transformers
            transformers-compat
            unordered-containers
            wai
            wai-websockets
            websockets
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ocharles/engine.io";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      entropy = callPackage ({ Cabal, base, bytestring, directory, filepath, mkDerivation, process, stdenv, unix }:
      mkDerivation {
          pname = "entropy";
          version = "0.3.7";
          sha256 = "1ff020eba2edbb93c4b23297470f8c11d69d0ff1e1642d17cbab9d54a24befef";
          revision = "1";
          editedCabalFile = "01lyh4cbpqlcj1y8mnkw6vk4vid5rzqg1vcf9kwxwd88zj86cgjg";
          setupHaskellDepends = [
            base
            Cabal
            directory
            filepath
            process
          ];
          libraryHaskellDepends = [
            base
            bytestring
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/TomMD/entropy";
          description = "A platform independent entropy source";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      erf = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "erf";
          version = "2.0.0.0";
          sha256 = "24f0b79c7e1d25cb2cd44c2258d7a464bf6db8079775b50b60b54a254616b337";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "The error function, erf, and related functions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      errors = callPackage ({ base, exceptions, mkDerivation, safe, stdenv, text, transformers, transformers-compat, unexceptionalio }:
      mkDerivation {
          pname = "errors";
          version = "2.2.1";
          sha256 = "4197eb87910bf32d81b92247c2f8fbeee1650217532374ecb63a09d12255f531";
          libraryHaskellDepends = [
            base
            exceptions
            safe
            text
            transformers
            transformers-compat
            unexceptionalio
          ];
          doHaddock = false;
          doCheck = false;
          description = "Simplified error-handling";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ether = callPackage ({ base, exceptions, mkDerivation, mmorph, monad-control, mtl, reflection, stdenv, tagged, template-haskell, transformers, transformers-base, transformers-lift, writer-cps-mtl }:
      mkDerivation {
          pname = "ether";
          version = "0.5.1.0";
          sha256 = "36980c9598c5e8e804695da3b966416a2221296022b39be437ec35263ea10085";
          libraryHaskellDepends = [
            base
            exceptions
            mmorph
            monad-control
            mtl
            reflection
            tagged
            template-haskell
            transformers
            transformers-base
            transformers-lift
            writer-cps-mtl
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://int-index.github.io/ether/";
          description = "Monad transformers and classes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      exceptions = callPackage ({ base, mkDerivation, mtl, stdenv, stm, template-haskell, transformers, transformers-compat }:
      mkDerivation {
          pname = "exceptions";
          version = "0.8.3";
          sha256 = "4d6ad97e8e3d5dc6ce9ae68a469dc2fd3f66e9d312bc6faa7ab162eddcef87be";
          revision = "2";
          editedCabalFile = "1vl59j0l7m53hkzlcfmdbqbab8dk4lp9gzwryn7nsr6ylg94wayw";
          libraryHaskellDepends = [
            base
            mtl
            stm
            template-haskell
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/exceptions/";
          description = "Extensible optionally-pure exceptions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      extensible-exceptions = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "extensible-exceptions";
          version = "0.1.1.4";
          sha256 = "6ce5e8801760385a408dab71b53550f87629e661b260bdc2cd41c6a439b6e388";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Extensible exceptions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      extra = callPackage ({ base, clock, directory, filepath, mkDerivation, process, stdenv, time, unix }:
      mkDerivation {
          pname = "extra";
          version = "1.5.3";
          sha256 = "a44b5db0c7004a9299f738e30e4aa4cac1e4428a84fb67fd9b1b21f96fd58c70";
          libraryHaskellDepends = [
            base
            clock
            directory
            filepath
            process
            time
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ndmitchell/extra#readme";
          description = "Extra functions I use";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      fail = callPackage ({ mkDerivation, stdenv }:
      mkDerivation {
          pname = "fail";
          version = "4.9.0.0";
          sha256 = "6d5cdb1a5c539425a9665f740e364722e1d9d6ae37fbc55f30fe3dbbbb91d4a2";
          doHaddock = false;
          doCheck = false;
          homepage = "https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail";
          description = "Forward-compatible MonadFail class";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      fast-logger = callPackage ({ array, auto-update, base, bytestring, directory, easy-file, filepath, mkDerivation, stdenv, text, unix, unix-time }:
      mkDerivation {
          pname = "fast-logger";
          version = "2.4.10";
          sha256 = "dec4a5d1a88f822d08d334ee870a08a8bb63b2b226d145cd24a7f08676ce678d";
          libraryHaskellDepends = [
            array
            auto-update
            base
            bytestring
            directory
            easy-file
            filepath
            text
            unix
            unix-time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/kazu-yamamoto/logger";
          description = "A fast logging system";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      fgl = callPackage ({ array, base, containers, deepseq, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "fgl";
          version = "5.5.3.1";
          sha256 = "dea97201d22c55b57a38b8f5a1ff272be8ba83db3824ab0f1232c60b8dcc2e4c";
          revision = "1";
          editedCabalFile = "00bw87y97ym844ir4mdq0vx5kfb0brzlqmrbqa0iq35lkwsd4k3g";
          libraryHaskellDepends = [
            array
            base
            containers
            deepseq
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          description = "Martin Erwig's Functional Graph Library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      file-embed = callPackage ({ base, bytestring, directory, filepath, mkDerivation, stdenv, template-haskell }:
      mkDerivation {
          pname = "file-embed";
          version = "0.0.10";
          sha256 = "f751925cec5773a4fad5a48ca0a86a21091ee5f1efccf618a64a89fa2cf5f711";
          libraryHaskellDepends = [
            base
            bytestring
            directory
            filepath
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/snoyberg/file-embed";
          description = "Use Template Haskell to embed file contents directly";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      filelock = callPackage ({ base, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "filelock";
          version = "0.1.1.2";
          sha256 = "0ff1dcb13ec619f72496035e2a1298ef9dc6a814ba304d882cd9b145eae3203d";
          libraryHaskellDepends = [
            base
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/takano-akio/filelock";
          description = "Portable interface to file locking (flock / LockFileEx)";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      filepath = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "filepath";
          version = "1.4.1.1";
          sha256 = "52fdbde3bc3a44d920544b8d184bd7241bac3f92d1fc6e299d716e06e99f12b4";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/filepath#readme";
          description = "Library for manipulating FilePaths in a cross platform way";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      fingertree = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "fingertree";
          version = "0.1.1.0";
          sha256 = "160c5ba370d781dbf2920ddca870ce8596ab76729972535595bef835ee1cddf0";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Generic finger-tree structure, with example instances";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      focus = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "focus";
          version = "0.1.5.2";
          sha256 = "c2988d48c2bc6861a00be4e02161df96abcbf6c80e801676cee39b7628715cb7";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nikita-volkov/focus";
          description = "A general abstraction for manipulating elements of container data structures";
          license = stdenv.lib.licenses.mit;
        }) {};
      foldl = callPackage ({ base, bytestring, comonad, containers, contravariant, hashable, mkDerivation, mwc-random, primitive, profunctors, stdenv, text, transformers, unordered-containers, vector }:
      mkDerivation {
          pname = "foldl";
          version = "1.2.5";
          sha256 = "aa2d5c3cfb8641163dcdd489e9e0fe481301e94c0e3940fc9e234f8e1b00ec4b";
          revision = "1";
          editedCabalFile = "02lk5838594mi15bylz2kpcm1c4akbsswj73i7k8xw4ns66iaq04";
          libraryHaskellDepends = [
            base
            bytestring
            comonad
            containers
            contravariant
            hashable
            mwc-random
            primitive
            profunctors
            text
            transformers
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          description = "Composable, streaming, and efficient left folds";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      formatting = callPackage ({ base, clock, mkDerivation, old-locale, scientific, stdenv, text, text-format, time }:
      mkDerivation {
          pname = "formatting";
          version = "6.2.4";
          sha256 = "432db74037d3bc326ab70e6e033502f818d9694535dbfc4c949cb50f72f33367";
          libraryHaskellDepends = [
            base
            clock
            old-locale
            scientific
            text
            text-format
            time
          ];
          doHaddock = false;
          doCheck = false;
          description = "Combinator-based type-safe formatting (like printf() or FORMAT)";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      foundation = callPackage ({ base, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "foundation";
          version = "0.0.13";
          sha256 = "106a85cbbf936591df44b46ee04d39f29c15752f6eca438341f2b735e9c0755f";
          libraryHaskellDepends = [
            base
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-foundation/foundation";
          description = "Alternative prelude with batteries and no dependencies";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      free = callPackage ({ base, bifunctors, comonad, containers, distributive, exceptions, mkDerivation, mtl, prelude-extras, profunctors, semigroupoids, semigroups, stdenv, template-haskell, transformers, transformers-compat }:
      mkDerivation {
          pname = "free";
          version = "4.12.4";
          sha256 = "c9fe45aae387855626ecb5a0fea6afdb207143cb00af3b1f715d1032d2d08784";
          libraryHaskellDepends = [
            base
            bifunctors
            comonad
            containers
            distributive
            exceptions
            mtl
            prelude-extras
            profunctors
            semigroupoids
            semigroups
            template-haskell
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/free/";
          description = "Monads for free";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      fsnotify = callPackage ({ async, base, containers, directory, filepath, hinotify, mkDerivation, stdenv, text, time, unix-compat }:
      mkDerivation {
          pname = "fsnotify";
          version = "0.2.1.1";
          sha256 = "175a75962ad07c30c031fa8931f8d3e32abc06a96676e73e65cb7207e9d2dc90";
          libraryHaskellDepends = [
            async
            base
            containers
            directory
            filepath
            hinotify
            text
            time
            unix-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-fswatch/hfsnotify";
          description = "Cross platform library for file change notification";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      generic-arbitrary = callPackage ({ QuickCheck, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "generic-arbitrary";
          version = "0.1.0";
          sha256 = "69f30a54e7a3d0a45288778e22e6d0d03cfc3b525dfe0a663cd4f559a619bcc6";
          libraryHaskellDepends = [
            base
            QuickCheck
          ];
          doHaddock = false;
          doCheck = false;
          description = "Generic implementation for QuickCheck's Arbitrary";
          license = stdenv.lib.licenses.mit;
        }) {};
      generic-deriving = callPackage ({ base, containers, ghc-prim, mkDerivation, stdenv, template-haskell }:
      mkDerivation {
          pname = "generic-deriving";
          version = "1.11.2";
          sha256 = "29960f2aa810abffc2f02658e7fa523cbfa4c92102e02d252482f9551bc122f9";
          libraryHaskellDepends = [
            base
            containers
            ghc-prim
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/dreixel/generic-deriving";
          description = "Generic programming library for generalised deriving";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      generics-sop = callPackage ({ base, deepseq, ghc-prim, mkDerivation, stdenv, template-haskell }:
      mkDerivation {
          pname = "generics-sop";
          version = "0.3.1.0";
          sha256 = "a7836bb3ac44caab98775d1ee2bdf9aa42cbc8e90d4a42da3e9bb35e1fa45fad";
          libraryHaskellDepends = [
            base
            deepseq
            ghc-prim
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          description = "Generic Programming using True Sums of Products";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ghc-boot-th = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "ghc-boot-th";
          version = "8.0.2";
          sha256 = "5d00e271f2dd83ff2c69df4d3c17ced26eaffd5a65898b2a04b0dc75f99bf8f0";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Shared functionality between GHC and the @template-haskell@ library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ghc-prim = callPackage ({ mkDerivation, rts, stdenv }:
      mkDerivation {
          pname = "ghc-prim";
          version = "0.5.0.0";
          sha256 = "44bbe4f0858f5101d860b7447a689bcd38a2451f4cc1d29f0de130cbd92bd6b2";
          libraryHaskellDepends = [ rts ];
          doHaddock = false;
          doCheck = false;
          description = "GHC primitives";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      gitrev = callPackage ({ base, base-compat, directory, filepath, mkDerivation, process, stdenv, template-haskell }:
      mkDerivation {
          pname = "gitrev";
          version = "1.3.1";
          sha256 = "a89964db24f56727b0e7b10c98fe7c116d721d8c46f52d6e77088669aaa38332";
          libraryHaskellDepends = [
            base
            base-compat
            directory
            filepath
            process
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/acfoltzer/gitrev";
          description = "Compile git revision info into Haskell projects";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      graphviz = callPackage ({ base, bytestring, colour, containers, directory, dlist, fgl, filepath, mkDerivation, polyparse, process, stdenv, temporary, text, transformers, wl-pprint-text }:
      mkDerivation {
          pname = "graphviz";
          version = "2999.18.1.2";
          sha256 = "b08c2026d3810c15f6ad49a07fd7b879978d958fa477b369b719ec00741c85fc";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            bytestring
            colour
            containers
            directory
            dlist
            fgl
            filepath
            polyparse
            process
            temporary
            text
            transformers
            wl-pprint-text
          ];
          executableHaskellDepends = [
            base
            bytestring
            directory
            filepath
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://projects.haskell.org/graphviz/";
          description = "Bindings to Graphviz for graph visualisation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      groups = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "groups";
          version = "0.4.0.0";
          sha256 = "d328395164033e310148d57d5be86fc6cc4dbc97b4296b91f235b213cc80e8ce";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Haskell 98 groups";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      half = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "half";
          version = "0.2.2.3";
          sha256 = "85c244c80d1c889a3d79073a6f5a99d9e769dbe3c574ca11d992b2b4f7599a5c";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/half";
          description = "Half-precision floating-point";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      happy = callPackage ({ Cabal, array, base, containers, directory, filepath, mkDerivation, mtl, stdenv }:
      mkDerivation {
          pname = "happy";
          version = "1.19.8";
          sha256 = "4df739965d559e48a9b0044fa6140241c07e8f3c794c6c0a6323024fd7f0d3a0";
          isLibrary = false;
          isExecutable = true;
          setupHaskellDepends = [
            base
            Cabal
            directory
            filepath
          ];
          executableHaskellDepends = [
            array
            base
            containers
            mtl
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://www.haskell.org/happy/";
          description = "Happy is a parser generator for Haskell";
          license = stdenv.lib.licenses.bsd2;
        }) {};
      hashable = callPackage ({ base, bytestring, deepseq, ghc-prim, integer-gmp, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "hashable";
          version = "1.2.6.1";
          sha256 = "94ca8789e13bc05c1582c46b709f3b0f5aeec2092be634b8606dbd9c5915bb7a";
          revision = "2";
          editedCabalFile = "0w4756sa04nk2bw3vnysb0y9d09zzg3c77aydkjfxz1hnl1dvnjn";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            bytestring
            deepseq
            ghc-prim
            integer-gmp
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/tibbe/hashable";
          description = "A class for types that can be converted to a hash value";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      heaps = callPackage ({ Cabal, base, cabal-doctest, mkDerivation, stdenv }:
      mkDerivation {
          pname = "heaps";
          version = "0.3.5";
          sha256 = "1b13a38f3c522afb017ec58ef9e8d871511b780c7658360c8b1922a7e9d336dc";
          revision = "1";
          editedCabalFile = "05avm1b16gj3rlm9sjqkxb0flq055r6gqhnacp7yzw4j1bghm5j7";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/heaps/";
          description = "Asymptotically optimal Brodal/Okasaki heaps";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      hex = callPackage ({ base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "hex";
          version = "0.1.2";
          sha256 = "12ee1243edd80570a486521565fb0c9b5e39374f21a12f050636e71d55ec61ec";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          description = "Convert strings into hexadecimal and back";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      hinotify = callPackage ({ async, base, containers, directory, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "hinotify";
          version = "0.3.9";
          sha256 = "f2480e4c08a516831c2221eebc6a9d3242e892932d9315c34cbe92a101c5df99";
          libraryHaskellDepends = [
            async
            base
            containers
            directory
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/kolmodin/hinotify.git";
          description = "Haskell binding to inotify";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      hourglass = callPackage ({ base, deepseq, mkDerivation, stdenv }:
      mkDerivation {
          pname = "hourglass";
          version = "0.2.10";
          sha256 = "d553362d7a6f7df60d8ff99304aaad0995be81f9d302725ebe9441829a0f8d80";
          libraryHaskellDepends = [
            base
            deepseq
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/vincenthz/hs-hourglass";
          description = "simple performant time related library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      hscolour = callPackage ({ base, containers, mkDerivation, stdenv }:
      mkDerivation {
          pname = "hscolour";
          version = "1.24.4";
          sha256 = "243332b082294117f37b2c2c68079fa61af68b36223b3fc07594f245e0e5321d";
          isLibrary = true;
          isExecutable = true;
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            base
            containers
          ];
          executableHaskellDepends = [
            base
            containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://code.haskell.org/~malcolm/hscolour/";
          description = "Colourise Haskell code";
          license = "LGPL";
        }) {};
      hspec = callPackage ({ HUnit, QuickCheck, base, call-stack, hspec-core, hspec-discover, hspec-expectations, mkDerivation, stdenv, stringbuilder, transformers }:
      mkDerivation {
          pname = "hspec";
          version = "2.4.4";
          sha256 = "b01a3245da9c597608befddc4fc3cae35e5bc753235877076f11ae8e0647cf21";
          libraryHaskellDepends = [
            base
            call-stack
            hspec-core
            hspec-discover
            hspec-expectations
            HUnit
            QuickCheck
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://hspec.github.io/";
          description = "A Testing Framework for Haskell";
          license = stdenv.lib.licenses.mit;
        }) {};
      hspec-core = callPackage ({ HUnit, QuickCheck, ansi-terminal, array, async, base, call-stack, deepseq, directory, filepath, hspec-expectations, mkDerivation, quickcheck-io, random, setenv, stdenv, tf-random, time, transformers }:
      mkDerivation {
          pname = "hspec-core";
          version = "2.4.4";
          sha256 = "601d321cdf7f2685880ee80c31154763884cb90dc512906005c4a485e8c8bfdf";
          libraryHaskellDepends = [
            ansi-terminal
            array
            async
            base
            call-stack
            deepseq
            directory
            filepath
            hspec-expectations
            HUnit
            QuickCheck
            quickcheck-io
            random
            setenv
            tf-random
            time
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://hspec.github.io/";
          description = "A Testing Framework for Haskell";
          license = stdenv.lib.licenses.mit;
        }) {};
      hspec-discover = callPackage ({ base, directory, filepath, mkDerivation, stdenv }:
      mkDerivation {
          pname = "hspec-discover";
          version = "2.4.4";
          sha256 = "76423bc72f3ed0a80ccefb26fbf3fb16c3d74a69d69b4ce0bc88db54984d5d47";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            directory
            filepath
          ];
          executableHaskellDepends = [
            base
            directory
            filepath
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://hspec.github.io/";
          description = "Automatically discover and run Hspec tests";
          license = stdenv.lib.licenses.mit;
        }) {};
      hspec-expectations = callPackage ({ HUnit, base, call-stack, mkDerivation, stdenv }:
      mkDerivation {
          pname = "hspec-expectations";
          version = "0.8.2";
          sha256 = "819607ea1faf35ce5be34be61c6f50f3389ea43892d56fb28c57a9f5d54fb4ef";
          libraryHaskellDepends = [
            base
            call-stack
            HUnit
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hspec/hspec-expectations#readme";
          description = "Catchy combinators for HUnit";
          license = stdenv.lib.licenses.mit;
        }) {};
      http-api-data = callPackage ({ Cabal, attoparsec, attoparsec-iso8601, base, bytestring, cabal-doctest, containers, hashable, http-types, mkDerivation, stdenv, text, time, time-locale-compat, unordered-containers, uri-bytestring, uuid-types }:
      mkDerivation {
          pname = "http-api-data";
          version = "0.3.7.1";
          sha256 = "8c633e95113c8ab655f4ba67e51e41a2c9035f0122ea68bfbb876b37277075fd";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            attoparsec
            attoparsec-iso8601
            base
            bytestring
            containers
            hashable
            http-types
            text
            time
            time-locale-compat
            unordered-containers
            uri-bytestring
            uuid-types
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/fizruk/http-api-data";
          description = "Converting to/from HTTP API data like URL pieces, headers and query parameters";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      http-client = callPackage ({ array, base, base64-bytestring, blaze-builder, bytestring, case-insensitive, containers, cookie, deepseq, exceptions, filepath, ghc-prim, http-types, mime-types, mkDerivation, network, network-uri, random, stdenv, streaming-commons, text, time, transformers }:
      mkDerivation {
          pname = "http-client";
          version = "0.5.7.0";
          sha256 = "e241eac22c55cd851de534b9d84149702a8b3990d10afc282bd6955df550ffa3";
          libraryHaskellDepends = [
            array
            base
            base64-bytestring
            blaze-builder
            bytestring
            case-insensitive
            containers
            cookie
            deepseq
            exceptions
            filepath
            ghc-prim
            http-types
            mime-types
            network
            network-uri
            random
            streaming-commons
            text
            time
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/snoyberg/http-client";
          description = "An HTTP client engine";
          license = stdenv.lib.licenses.mit;
        }) {};
      http-client-tls = callPackage ({ base, bytestring, case-insensitive, connection, containers, cryptonite, data-default-class, exceptions, http-client, http-types, memory, mkDerivation, network, network-uri, stdenv, text, tls, transformers }:
      mkDerivation {
          pname = "http-client-tls";
          version = "0.3.5.1";
          sha256 = "c1fa23eb868f4b4e36304f3d03890bce1230284be79f80bd7b4fe1733e8a9558";
          libraryHaskellDepends = [
            base
            bytestring
            case-insensitive
            connection
            containers
            cryptonite
            data-default-class
            exceptions
            http-client
            http-types
            memory
            network
            network-uri
            text
            tls
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/snoyberg/http-client";
          description = "http-client backend using the connection package and tls library";
          license = stdenv.lib.licenses.mit;
        }) {};
      http-conduit = callPackage ({ aeson, base, bytestring, conduit, conduit-extra, exceptions, http-client, http-client-tls, http-types, lifted-base, mkDerivation, monad-control, mtl, resourcet, stdenv, transformers }:
      mkDerivation {
          pname = "http-conduit";
          version = "2.2.3.2";
          sha256 = "e359c3ef370731e895a5c213e805c6806394f13598647f36dce7be41d4c41eb8";
          libraryHaskellDepends = [
            aeson
            base
            bytestring
            conduit
            conduit-extra
            exceptions
            http-client
            http-client-tls
            http-types
            lifted-base
            monad-control
            mtl
            resourcet
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.yesodweb.com/book/http-conduit";
          description = "HTTP client package with conduit interface and HTTPS support";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      http-date = callPackage ({ array, attoparsec, base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "http-date";
          version = "0.0.6.1";
          sha256 = "f2e106603e2b3f710f1189e478f6c20067d9a9d21a20a633fe362b3f91807636";
          libraryHaskellDepends = [
            array
            attoparsec
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          description = "HTTP Date parser/formatter";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      http-media = callPackage ({ base, bytestring, case-insensitive, containers, mkDerivation, stdenv }:
      mkDerivation {
          pname = "http-media";
          version = "0.6.4";
          sha256 = "ef762cf50854250e4247b744decbebe4d3d188dbc19bfd90aa344ed3c61cc9d3";
          libraryHaskellDepends = [
            base
            bytestring
            case-insensitive
            containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/zmthy/http-media";
          description = "Processing HTTP Content-Type and Accept headers";
          license = stdenv.lib.licenses.mit;
        }) {};
      http-types = callPackage ({ array, base, blaze-builder, bytestring, case-insensitive, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "http-types";
          version = "0.9.1";
          sha256 = "7bed648cdc1c69e76bf039763dbe1074b55fd2704911dd0cb6b7dfebf1b6f550";
          libraryHaskellDepends = [
            array
            base
            blaze-builder
            bytestring
            case-insensitive
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/aristidb/http-types";
          description = "Generic HTTP types for Haskell (for both client and server code)";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      http2 = callPackage ({ aeson, aeson-pretty, array, base, bytestring, bytestring-builder, case-insensitive, containers, directory, filepath, hex, mkDerivation, psqueues, stdenv, stm, text, unordered-containers, vector, word8 }:
      mkDerivation {
          pname = "http2";
          version = "1.6.3";
          sha256 = "61620eca0f57875a6a9bd24f9cc04c301b5c3c668bf98f85e9989aad5d069c43";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            array
            base
            bytestring
            bytestring-builder
            case-insensitive
            containers
            psqueues
            stm
          ];
          executableHaskellDepends = [
            aeson
            aeson-pretty
            array
            base
            bytestring
            bytestring-builder
            case-insensitive
            containers
            directory
            filepath
            hex
            text
            unordered-containers
            vector
            word8
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/kazu-yamamoto/http2";
          description = "HTTP/2 library including frames, priority queues and HPACK";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      insert-ordered-containers = callPackage ({ aeson, base, base-compat, hashable, lens, mkDerivation, semigroupoids, semigroups, stdenv, text, transformers, unordered-containers }:
      mkDerivation {
          pname = "insert-ordered-containers";
          version = "0.2.1.0";
          sha256 = "d71d126bf455898492e1d2ba18b2ad04453f8b0e4daff3926a67f0560a712298";
          revision = "3";
          editedCabalFile = "0ik4n32rvamxvlp80ixjrbhskivynli7b89s4hk6401bcy3ykp3g";
          libraryHaskellDepends = [
            aeson
            base
            base-compat
            hashable
            lens
            semigroupoids
            semigroups
            text
            transformers
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/insert-ordered-containers#readme";
          description = "Associative containers retating insertion order for traversals";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      integer-gmp = callPackage ({ ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "integer-gmp";
          version = "1.0.0.1";
          sha256 = "ef11daab7d7007b6be61846350a947173c46475c833623bcac45aa532ec3c121";
          revision = "1";
          editedCabalFile = "1mfl651b2v82qhm5h279mjhq4ilzf6x1yydi3npa10ja6isifvb1";
          libraryHaskellDepends = [
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          description = "Integer library based on GMP";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      integer-logarithms = callPackage ({ array, base, ghc-prim, integer-gmp, mkDerivation, stdenv }:
      mkDerivation {
          pname = "integer-logarithms";
          version = "1.0.2";
          sha256 = "31069ccbff489baf6c4a93cb7475640aabea9366eb0b583236f10714a682b570";
          libraryHaskellDepends = [
            array
            base
            ghc-prim
            integer-gmp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/integer-logarithms";
          description = "Integer logarithms";
          license = stdenv.lib.licenses.mit;
        }) {};
      intervals = callPackage ({ Cabal, array, base, cabal-doctest, distributive, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "intervals";
          version = "0.8.1";
          sha256 = "9ce3bf9d31b9ab2296fccc25031fd52e1c3e4abeca5d3bb452a725b586eb7e03";
          revision = "1";
          editedCabalFile = "0im2m4acx6g638h7yz0x3qyaipfmri59q4zdq1w7n608r3i406dj";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            array
            base
            distributive
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/intervals";
          description = "Interval Arithmetic";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      iproute = callPackage ({ appar, base, byteorder, containers, mkDerivation, network, stdenv }:
      mkDerivation {
          pname = "iproute";
          version = "1.7.1";
          sha256 = "57b8d03ca8ce92f8ec1334564f3edff53a0621ccbc43c00ba02eaa5007ee3eee";
          libraryHaskellDepends = [
            appar
            base
            byteorder
            containers
            network
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.mew.org/~kazu/proj/iproute/";
          description = "IP Routing Table";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      jailbreak-cabal = callPackage ({ Cabal, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "jailbreak-cabal";
          version = "1.3.2";
          sha256 = "212a8bbc3dfc748c4063282414a2726709d651322f3984c9989179d2352950f4";
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [
            base
            Cabal
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/peti/jailbreak-cabal#readme";
          description = "Strip version restrictions from build dependencies in Cabal files";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      kademlia = callPackage ({ MonadRandom, base, binary, bytestring, containers, contravariant, cryptonite, data-default, extra, fetchgit, memory, mkDerivation, mtl, network, random, random-shuffle, stdenv, stm, time, transformers, transformers-compat }:
      mkDerivation {
          pname = "kademlia";
          version = "1.1.0.1";
          src = fetchgit {
            url = "https://github.com/serokell/kademlia.git";
            sha256 = "1k1wp9dwhzzqfivxc28vhxfqplnyh916crr7bhsiv829d6qifhw1";
            rev = "7120bb4d28e708acd52dfd61d3dca7914fac7d7f";
          };
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            bytestring
            containers
            contravariant
            cryptonite
            extra
            memory
            MonadRandom
            mtl
            network
            random
            random-shuffle
            stm
            time
            transformers
          ];
          executableHaskellDepends = [
            base
            binary
            bytestring
            containers
            data-default
            extra
            MonadRandom
            mtl
            network
            random
            random-shuffle
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/serokell/kademlia";
          description = "An implementation of the Kademlia DHT Protocol";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      kan-extensions = callPackage ({ adjunctions, array, base, comonad, containers, contravariant, distributive, fail, free, mkDerivation, mtl, semigroupoids, stdenv, tagged, transformers }:
      mkDerivation {
          pname = "kan-extensions";
          version = "5.0.2";
          sha256 = "1c9ede8595424209944e59fd46c1d2edb654758be9a45c1c48a4d3cedf42482e";
          libraryHaskellDepends = [
            adjunctions
            array
            base
            comonad
            containers
            contravariant
            distributive
            fail
            free
            mtl
            semigroupoids
            tagged
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/kan-extensions/";
          description = "Kan extensions, Kan lifts, various forms of the Yoneda lemma, and (co)density (co)monads";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      lens = callPackage ({ Cabal, array, base, base-orphans, bifunctors, bytestring, cabal-doctest, call-stack, comonad, containers, contravariant, distributive, exceptions, filepath, free, ghc-prim, hashable, kan-extensions, mkDerivation, mtl, parallel, profunctors, reflection, semigroupoids, semigroups, stdenv, tagged, template-haskell, text, th-abstraction, transformers, transformers-compat, unordered-containers, vector, void }:
      mkDerivation {
          pname = "lens";
          version = "4.15.4";
          sha256 = "742e7b87d7945e3d9c1d39d3f8440094c0a31cd098f06a08f8dabefba0a57cd2";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
            filepath
          ];
          libraryHaskellDepends = [
            array
            base
            base-orphans
            bifunctors
            bytestring
            call-stack
            comonad
            containers
            contravariant
            distributive
            exceptions
            filepath
            free
            ghc-prim
            hashable
            kan-extensions
            mtl
            parallel
            profunctors
            reflection
            semigroupoids
            semigroups
            tagged
            template-haskell
            text
            th-abstraction
            transformers
            transformers-compat
            unordered-containers
            vector
            void
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/lens/";
          description = "Lenses, Folds and Traversals";
          license = stdenv.lib.licenses.bsd2;
        }) {};
      lifted-async = callPackage ({ async, base, constraints, lifted-base, mkDerivation, monad-control, stdenv, transformers-base }:
      mkDerivation {
          pname = "lifted-async";
          version = "0.9.3";
          sha256 = "97978307f34c8ab1d765724d723a13fede4112a94fe5fbf3494f00378961b461";
          libraryHaskellDepends = [
            async
            base
            constraints
            lifted-base
            monad-control
            transformers-base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/maoe/lifted-async";
          description = "Run lifted IO operations asynchronously and wait for their results";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      lifted-base = callPackage ({ base, mkDerivation, monad-control, stdenv, transformers-base }:
      mkDerivation {
          pname = "lifted-base";
          version = "0.2.3.11";
          sha256 = "8ec47a9fce7cf5913766a5c53e1b2cf254be733fa9d62e6e2f3f24e538005aab";
          libraryHaskellDepends = [
            base
            monad-control
            transformers-base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/lifted-base";
          description = "lifted IO operations from the base library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      linear = callPackage ({ Cabal, adjunctions, base, base-orphans, binary, bytes, cabal-doctest, cereal, containers, deepseq, distributive, ghc-prim, hashable, lens, mkDerivation, reflection, semigroupoids, semigroups, stdenv, tagged, template-haskell, transformers, transformers-compat, unordered-containers, vector, void }:
      mkDerivation {
          pname = "linear";
          version = "1.20.7";
          sha256 = "4b88b6268d327220a296b6790c82db8ebab52973735af0a9de1c734cdc07cab6";
          revision = "1";
          editedCabalFile = "0ghmlkk5cy0pylx47rwr37p403ml7x6sg0sapz9c7534nzzhxq0g";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            adjunctions
            base
            base-orphans
            binary
            bytes
            cereal
            containers
            deepseq
            distributive
            ghc-prim
            hashable
            lens
            reflection
            semigroupoids
            semigroups
            tagged
            template-haskell
            transformers
            transformers-compat
            unordered-containers
            vector
            void
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/linear/";
          description = "Linear Algebra";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      list-t = callPackage ({ base, base-prelude, mkDerivation, mmorph, monad-control, mtl, stdenv, transformers, transformers-base }:
      mkDerivation {
          pname = "list-t";
          version = "1.0.0.1";
          sha256 = "4a4929b3733e692dd8072cc8521691dcc5e207f2218fe0201b9285641df8f701";
          libraryHaskellDepends = [
            base
            base-prelude
            mmorph
            monad-control
            mtl
            transformers
            transformers-base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nikita-volkov/list-t";
          description = "ListT done right";
          license = stdenv.lib.licenses.mit;
        }) {};
      log-warper = callPackage ({ aeson, ansi-terminal, base, containers, deepseq, directory, dlist, errors, exceptions, extra, filepath, formatting, hashable, lens, mkDerivation, mmorph, monad-control, monad-loops, mtl, network, safecopy, stdenv, text, text-format, time, transformers, transformers-base, universum, unix, unordered-containers, yaml }:
      mkDerivation {
          pname = "log-warper";
          version = "1.2.3.1";
          sha256 = "47dd0a5a3209290527d9d4c329267a1ac8dcd976f1e2bd6a4062a2ff810a163b";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            ansi-terminal
            base
            containers
            deepseq
            directory
            dlist
            errors
            exceptions
            extra
            filepath
            formatting
            hashable
            lens
            mmorph
            monad-control
            monad-loops
            mtl
            network
            safecopy
            text
            text-format
            time
            transformers
            transformers-base
            universum
            unix
            unordered-containers
            yaml
          ];
          executableHaskellDepends = [
            base
            exceptions
            text
            universum
            yaml
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/serokell/log-warper";
          description = "Flexible, configurable, monadic and pretty logging";
          license = stdenv.lib.licenses.mit;
        }) {};
      lrucache = callPackage ({ base, containers, contravariant, mkDerivation, stdenv }:
      mkDerivation {
          pname = "lrucache";
          version = "1.2.0.0";
          sha256 = "5f17a9e026e198152d13830a0eae0df21be437c238a3f157f7c188fe27a37616";
          libraryHaskellDepends = [
            base
            containers
            contravariant
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/chowells79/lrucache";
          description = "a simple, pure LRU cache";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      math-functions = callPackage ({ base, deepseq, mkDerivation, primitive, stdenv, vector, vector-th-unbox }:
      mkDerivation {
          pname = "math-functions";
          version = "0.2.1.0";
          sha256 = "f71b5598de453546396a3f5f7f6ce877fffcc996639b7569d8628cae97da65eb";
          libraryHaskellDepends = [
            base
            deepseq
            primitive
            vector
            vector-th-unbox
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/math-functions";
          description = "Special functions and Chebyshev polynomials";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      memory = callPackage ({ base, bytestring, deepseq, foundation, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "memory";
          version = "0.14.6";
          sha256 = "c7dec070174756f1753010585a6dcd4f958a4360634142c4e387b3475bffc160";
          revision = "1";
          editedCabalFile = "0pyzdy5ca1cbkjzy1scnz6mr9251ap4w8a5phzxp91wkxpc45538";
          libraryHaskellDepends = [
            base
            bytestring
            deepseq
            foundation
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/vincenthz/hs-memory";
          description = "memory and related abstraction stuff";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      microlens = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "microlens";
          version = "0.4.8.1";
          sha256 = "17b8df1d3472463934edf1a519f23d8ef315693bda30d83c8c661936187e0a47";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/aelve/microlens";
          description = "A tiny lens library with no dependencies. If you're writing an app, you probably want microlens-platform, not this.";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      microlens-mtl = callPackage ({ base, microlens, mkDerivation, mtl, stdenv, transformers, transformers-compat }:
      mkDerivation {
          pname = "microlens-mtl";
          version = "0.1.11.0";
          sha256 = "4eba3fc8b776877cfcabc63418ee8307de274cc144792d70013bb3a7119b05a1";
          libraryHaskellDepends = [
            base
            microlens
            mtl
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/aelve/microlens";
          description = "microlens support for Reader/Writer/State from mtl";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      mime-types = callPackage ({ base, bytestring, containers, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "mime-types";
          version = "0.1.0.7";
          sha256 = "83164a24963a7ef37543349df095155b30116c208e602a159a5cd3722f66e9b9";
          libraryHaskellDepends = [
            base
            bytestring
            containers
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/yesodweb/wai";
          description = "Basic mime-type handling types and functions";
          license = stdenv.lib.licenses.mit;
        }) {};
      mmorph = callPackage ({ base, mkDerivation, mtl, stdenv, transformers, transformers-compat }:
      mkDerivation {
          pname = "mmorph";
          version = "1.0.9";
          sha256 = "e1f27d3881b254e2a87ffb21f33e332404abb180361f9d29092a85e321554563";
          revision = "1";
          editedCabalFile = "1xxf78qi08qsis2q785s0ra29wjxnxw8pyns0dsqp4a6cybd3mjd";
          libraryHaskellDepends = [
            base
            mtl
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          description = "Monad morphisms";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      monad-control = callPackage ({ base, mkDerivation, stdenv, stm, transformers, transformers-base, transformers-compat }:
      mkDerivation {
          pname = "monad-control";
          version = "1.0.2.2";
          sha256 = "1e34a21d123f2ed8bb2708e7f30343fa1d9d7f36881f9871cbcca4bb07e7e433";
          libraryHaskellDepends = [
            base
            stm
            transformers
            transformers-base
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/monad-control";
          description = "Lift control operations, like exception catching, through monad transformers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      monad-loops = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "monad-loops";
          version = "0.4.3";
          sha256 = "7eaaaf6bc43661e9e86e310ff8c56fbea16eb6bf13c31a2e28103138ac164c18";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/mokus0/monad-loops";
          description = "Monadic loops";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      monad-par = callPackage ({ abstract-deque, abstract-par, array, base, containers, deepseq, mkDerivation, monad-par-extras, mtl, mwc-random, parallel, stdenv }:
      mkDerivation {
          pname = "monad-par";
          version = "0.3.4.8";
          sha256 = "f84cdf51908a1c41c3f672be9520a8fdc028ea39d90a25ecfe5a3b223cfeb951";
          libraryHaskellDepends = [
            abstract-deque
            abstract-par
            array
            base
            containers
            deepseq
            monad-par-extras
            mtl
            mwc-random
            parallel
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/simonmar/monad-par";
          description = "A library for parallel programming based on a monad";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      monad-par-extras = callPackage ({ abstract-par, base, cereal, deepseq, mkDerivation, mtl, random, stdenv, transformers }:
      mkDerivation {
          pname = "monad-par-extras";
          version = "0.3.3";
          sha256 = "e21e33190bc248afa4ae467287ac37d24037ef3de6050c44fd85b52f4d5b842e";
          libraryHaskellDepends = [
            abstract-par
            base
            cereal
            deepseq
            mtl
            random
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/simonmar/monad-par";
          description = "Combinators and extra features for Par monads";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      mono-traversable = callPackage ({ base, bytestring, containers, hashable, mkDerivation, split, stdenv, text, transformers, unordered-containers, vector, vector-algorithms }:
      mkDerivation {
          pname = "mono-traversable";
          version = "1.0.2.1";
          sha256 = "4ed2f4a2c389105b330b631eeff03e36908765ca120888922aeda819f9cdb16a";
          libraryHaskellDepends = [
            base
            bytestring
            containers
            hashable
            split
            text
            transformers
            unordered-containers
            vector
            vector-algorithms
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/snoyberg/mono-traversable";
          description = "Type classes for mapping, folding, and traversing monomorphic containers";
          license = stdenv.lib.licenses.mit;
        }) {};
      monoid-extras = callPackage ({ base, groups, mkDerivation, semigroupoids, semigroups, stdenv }:
      mkDerivation {
          pname = "monoid-extras";
          version = "0.4.2";
          sha256 = "13ff4e055c9656a3e599567cbc4a46ef8617c05534de46909a4239696e34281f";
          revision = "2";
          editedCabalFile = "04h78r48rg2ppi53869vb8y226g135fxgy9ryi1v08nqsiqi1vvw";
          libraryHaskellDepends = [
            base
            groups
            semigroupoids
            semigroups
          ];
          doHaddock = false;
          doCheck = false;
          description = "Various extra monoid-related definitions and utilities";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      mtl = callPackage ({ base, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "mtl";
          version = "2.2.1";
          sha256 = "cae59d79f3a16f8e9f3c9adc1010c7c6cdddc73e8a97ff4305f6439d855c8dc5";
          revision = "1";
          editedCabalFile = "0fsa965g9h23mlfjzghmmhcb9dmaq8zpm374gby6iwgdx47q0njb";
          libraryHaskellDepends = [
            base
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/mtl";
          description = "Monad classes, using functional dependencies";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      mwc-random = callPackage ({ base, math-functions, mkDerivation, primitive, stdenv, time, vector }:
      mkDerivation {
          pname = "mwc-random";
          version = "0.13.6.0";
          sha256 = "065f334fc13c057eb03ef0b6aa3665ff193609d9bfcad8068bdd260801f44716";
          libraryHaskellDepends = [
            base
            math-functions
            primitive
            time
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/mwc-random";
          description = "Fast, high quality pseudo random number generation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      natural-transformation = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "natural-transformation";
          version = "0.4";
          sha256 = "aac28e2c1147ed77c1ec0f0eb607a577fa26d0fd67474293ba860ec124efc8af";
          revision = "2";
          editedCabalFile = "1j90pd1zznr18966axskad5w0kx4dvqg62r65rmw1ihqwxm1ndix";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ku-fpg/natural-transformation";
          description = "A natural transformation package";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      neat-interpolation = callPackage ({ base, base-prelude, mkDerivation, parsec, stdenv, template-haskell, text }:
      mkDerivation {
          pname = "neat-interpolation";
          version = "0.3.2.1";
          sha256 = "5530e43ca4de09b972d173e522f9dc96265f3afe0df695a25f0141be816fa014";
          libraryHaskellDepends = [
            base
            base-prelude
            parsec
            template-haskell
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nikita-volkov/neat-interpolation";
          description = "A quasiquoter for neat and simple multiline text interpolation";
          license = stdenv.lib.licenses.mit;
        }) {};
      network = callPackage ({ base, bytestring, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "network";
          version = "2.6.3.2";
          sha256 = "354477074eaf2c0e134f4a7ae17694ffc747d484133463e95fae57ecbe48c0b6";
          revision = "1";
          editedCabalFile = "17234sy0vqic8g9wg8gmfmc0by50scjwbdk8bkcl9kjf3fvs4nyx";
          libraryHaskellDepends = [
            base
            bytestring
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/network";
          description = "Low-level networking interface";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      network-info = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "network-info";
          version = "0.2.0.8";
          sha256 = "ecdff121f3e154c480f363bc8660959a051790bfae5b5fe573810873cedbcd76";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/jystic/network-info";
          description = "Access the local computer's basic network configuration";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      network-transport = callPackage ({ base, binary, bytestring, deepseq, fetchgit, hashable, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "network-transport";
          version = "0.5.2";
          src = fetchgit {
            url = "https://github.com/serokell/network-transport";
            sha256 = "0lqa26l2ikpq6a4s7qm9b2favx59w82i0wngakhfyax66fpixp8q";
            rev = "018a50b9042c2115c3ec9c9fd5ca5f28737dd29c";
          };
          libraryHaskellDepends = [
            base
            binary
            bytestring
            deepseq
            hashable
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-distributed.github.com";
          description = "Network abstraction layer";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      network-transport-tcp = callPackage ({ base, bytestring, containers, data-accessor, fetchgit, mkDerivation, network, network-transport, stdenv, uuid }:
      mkDerivation {
          pname = "network-transport-tcp";
          version = "0.5.1";
          src = fetchgit {
            url = "https://github.com/serokell/network-transport-tcp";
            sha256 = "1l4df0wgaixslah2c05wvq3srdbw1rmq246889wn3r8h43l9i0wl";
            rev = "24dd213cef81d383083feb7f5d9ce4b8a6be8168";
          };
          libraryHaskellDepends = [
            base
            bytestring
            containers
            data-accessor
            network
            network-transport
            uuid
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-distributed.github.com";
          description = "TCP instantiation of Network.Transport";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      network-uri = callPackage ({ base, deepseq, mkDerivation, parsec, stdenv }:
      mkDerivation {
          pname = "network-uri";
          version = "2.6.1.0";
          sha256 = "423e0a2351236f3fcfd24e39cdbc38050ec2910f82245e69ca72a661f7fc47f0";
          revision = "1";
          editedCabalFile = "141nj7q0p9wkn5gr41ayc63cgaanr9m59yym47wpxqr3c334bk32";
          libraryHaskellDepends = [
            base
            deepseq
            parsec
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/network-uri";
          description = "URI manipulation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      newtype-generics = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "newtype-generics";
          version = "0.5";
          sha256 = "dc63ac2c9e682ee292a8f88fa3eb1af1b66d5860f7dcec0d09319c5ef96e7f9c";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "A typeclass and set of functions for working with newtypes, with generics support";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      node-sketch = callPackage ({ MonadRandom, QuickCheck, aeson, async, attoparsec, base, binary, bytestring, conduit, conduit-extra, containers, cryptonite, data-default, deepseq, ekg-core, exceptions, fetchgit, formatting, hashable, kademlia, lens, lifted-base, log-warper, mkDerivation, mmorph, monad-control, mtl, mwc-random, network, network-transport, network-transport-tcp, optparse-simple, random, resourcet, semigroups, serokell-util, statistics, stdenv, stm, tagged, text, text-format, time, time-units, transformers, transformers-base, transformers-lift, universum, unordered-containers, vector }:
      mkDerivation {
          pname = "node-sketch";
          version = "0.2.0.0";
          src = fetchgit {
            url = "https://github.com/arybczak/time-warp-nt.git";
            sha256 = "0aijj6wq14k0ralji2rn0qiyq8rplcpvgnv43w3vqdxikbxsg85s";
            rev = "74acf3c98ec402490e4ae086a60227560cde166d";
          };
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            async
            attoparsec
            base
            binary
            bytestring
            containers
            cryptonite
            data-default
            deepseq
            ekg-core
            exceptions
            formatting
            hashable
            kademlia
            lens
            lifted-base
            log-warper
            mmorph
            monad-control
            mtl
            mwc-random
            network
            network-transport
            network-transport-tcp
            QuickCheck
            random
            resourcet
            semigroups
            serokell-util
            statistics
            stm
            tagged
            text
            text-format
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unordered-containers
            vector
          ];
          executableHaskellDepends = [
            attoparsec
            base
            binary
            bytestring
            conduit
            conduit-extra
            containers
            exceptions
            formatting
            lens
            log-warper
            MonadRandom
            mtl
            network-transport-tcp
            optparse-simple
            random
            resourcet
            serokell-util
            stm
            text
            text-format
            time-units
          ];
          doHaddock = false;
          doCheck = false;
          license = stdenv.lib.licenses.mit;
        }) {};
      old-locale = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "old-locale";
          version = "1.0.0.7";
          sha256 = "dbaf8bf6b888fb98845705079296a23c3f40ee2f449df7312f7f7f1de18d7b50";
          revision = "2";
          editedCabalFile = "04b9vn007hlvsrx4ksd3r8r3kbyaj2kvwxchdrmd4370qzi8p6gs";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "locale library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      old-time = callPackage ({ base, mkDerivation, old-locale, stdenv }:
      mkDerivation {
          pname = "old-time";
          version = "1.1.0.3";
          sha256 = "1ccb158b0f7851715d36b757c523b026ca1541e2030d02239802ba39b4112bc1";
          revision = "2";
          editedCabalFile = "1j6ln1dkvhdvnwl33bp0xf9lhc4sybqk0aw42p8cq81xwwzbn7y9";
          libraryHaskellDepends = [
            base
            old-locale
          ];
          doHaddock = false;
          doCheck = false;
          description = "Time library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      operational = callPackage ({ base, mkDerivation, mtl, random, stdenv }:
      mkDerivation {
          pname = "operational";
          version = "0.2.3.5";
          sha256 = "91d479063ae7ed3d0a6ae911bdee550fbf31cf341910f9778046b484c55b4af4";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            mtl
          ];
          executableHaskellDepends = [
            base
            mtl
            random
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://wiki.haskell.org/Operational";
          description = "Implementation of difficult monads made easy with operational semantics";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      optparse-applicative = callPackage ({ ansi-wl-pprint, base, mkDerivation, process, stdenv, transformers, transformers-compat }:
      mkDerivation {
          pname = "optparse-applicative";
          version = "0.13.2.0";
          sha256 = "5c83cfce7e53f4d3b1f5d53f082e7e61959bf14e6be704c698c3ab7f1b956ca2";
          libraryHaskellDepends = [
            ansi-wl-pprint
            base
            process
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/pcapriotti/optparse-applicative";
          description = "Utilities and combinators for parsing command line options";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      optparse-simple = callPackage ({ base, either, gitrev, mkDerivation, optparse-applicative, stdenv, template-haskell, transformers }:
      mkDerivation {
          pname = "optparse-simple";
          version = "0.0.3";
          sha256 = "4547f0a6c1bd959b1d9c3c2a5fabee39ac9a19dffabbb2d75461a2d461df8c7e";
          libraryHaskellDepends = [
            base
            either
            gitrev
            optparse-applicative
            template-haskell
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          description = "Simple interface to optparse-applicative";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      parallel = callPackage ({ array, base, containers, deepseq, mkDerivation, stdenv }:
      mkDerivation {
          pname = "parallel";
          version = "3.2.1.1";
          sha256 = "323bb9bc9e36fb9bfb08e68a772411302b1599bfffbc6de20fa3437ce1473c17";
          libraryHaskellDepends = [
            array
            base
            containers
            deepseq
          ];
          doHaddock = false;
          doCheck = false;
          description = "Parallel programming library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      parsec = callPackage ({ base, bytestring, mkDerivation, mtl, stdenv, text }:
      mkDerivation {
          pname = "parsec";
          version = "3.1.11";
          sha256 = "6f87251cb1d11505e621274dec15972de924a9074f07f7430a18892064c2676e";
          libraryHaskellDepends = [
            base
            bytestring
            mtl
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/aslatter/parsec";
          description = "Monadic parser combinators";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pem = callPackage ({ base, base64-bytestring, bytestring, mkDerivation, mtl, stdenv }:
      mkDerivation {
          pname = "pem";
          version = "0.2.2";
          sha256 = "372808c76c6d860aedb4e30171cb4ee9f6154d9f68e3f2310f820bf174995a98";
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            base
            base64-bytestring
            bytestring
            mtl
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-pem";
          description = "Privacy Enhanced Mail (PEM) format reader and writer";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pipes = callPackage ({ base, exceptions, mkDerivation, mmorph, mtl, stdenv, transformers, void }:
      mkDerivation {
          pname = "pipes";
          version = "4.3.4";
          sha256 = "cb39ed76d3009a8ab9d30d0d0d5a5f22267a2ab7e69101fd8f4a3860bb275521";
          libraryHaskellDepends = [
            base
            exceptions
            mmorph
            mtl
            transformers
            void
          ];
          doHaddock = false;
          doCheck = false;
          description = "Compositional pipelines";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pipes-bytestring = callPackage ({ base, bytestring, mkDerivation, pipes, pipes-group, pipes-parse, stdenv, stringsearch, transformers }:
      mkDerivation {
          pname = "pipes-bytestring";
          version = "2.1.6";
          sha256 = "b1dc370680f3671759010caace183bce683d0481bd2c0e3f4906b78ac8623c18";
          libraryHaskellDepends = [
            base
            bytestring
            pipes
            pipes-group
            pipes-parse
            stringsearch
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          description = "ByteString support for pipes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pipes-group = callPackage ({ base, free, mkDerivation, pipes, pipes-parse, stdenv, transformers }:
      mkDerivation {
          pname = "pipes-group";
          version = "1.0.7";
          sha256 = "8643594e71ad75b8e7001e6456cf1e9fcdf999bb32a5b9fc0a88c81812730b5c";
          libraryHaskellDepends = [
            base
            free
            pipes
            pipes-parse
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          description = "Group streams into substreams";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pipes-interleave = callPackage ({ base, containers, heaps, mkDerivation, pipes, stdenv }:
      mkDerivation {
          pname = "pipes-interleave";
          version = "1.1.1";
          sha256 = "2758429d9da110fcd8037d2db301813c5635c28e89c01e91c709663d090aef50";
          libraryHaskellDepends = [
            base
            containers
            heaps
            pipes
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/bgamari/pipes-interleave";
          description = "Interleave and merge streams of elements";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pipes-parse = callPackage ({ base, mkDerivation, pipes, stdenv, transformers }:
      mkDerivation {
          pname = "pipes-parse";
          version = "3.0.8";
          sha256 = "d28f831b2c8229cca567ee95570787d2dd3f5cfcff3b3c44ee308360a8c107a9";
          libraryHaskellDepends = [
            base
            pipes
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          description = "Parsing infrastructure for the pipes ecosystem";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pipes-safe = callPackage ({ base, containers, exceptions, mkDerivation, monad-control, mtl, pipes, stdenv, transformers, transformers-base }:
      mkDerivation {
          pname = "pipes-safe";
          version = "2.2.5";
          sha256 = "0242cfe67853dc5bd94c979b06da25423d8bf96c3b095f4d33b745c78605a67c";
          libraryHaskellDepends = [
            base
            containers
            exceptions
            monad-control
            mtl
            pipes
            transformers
            transformers-base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Safety for the pipes ecosystem";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      plutus-prototype = callPackage ({ base, bifunctors, binary, bytestring, cardano-crypto, cryptonite, ed25519, either, fetchgit, filepath, lens, memory, mkDerivation, mtl, operational, parsec, stdenv, transformers }:
      mkDerivation {
          pname = "plutus-prototype";
          version = "0.1.0.0";
          src = fetchgit {
            url = "https://github.com/input-output-hk/plutus-prototype";
            sha256 = "1b0c9d8pr932fvaamyv53fa2jpfwm249imc8fxfybn71yz8p96ai";
            rev = "d4aa461fc69fc6957aab46b41a670c2144aefb77";
          };
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            base
            bifunctors
            binary
            bytestring
            cardano-crypto
            cryptonite
            ed25519
            either
            filepath
            lens
            memory
            mtl
            operational
            parsec
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "iohk.io";
          description = "Prototype of the Plutus language";
          license = stdenv.lib.licenses.mit;
        }) {};
      polyparse = callPackage ({ base, bytestring, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "polyparse";
          version = "1.12";
          sha256 = "f54c63584ace968381de4a06bd7328b6adc3e1a74fd336e18449e0dd7650be15";
          libraryHaskellDepends = [
            base
            bytestring
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://code.haskell.org/~malcolm/polyparse/";
          description = "A variety of alternative parser combinator libraries";
          license = "LGPL";
        }) {};
      prelude-extras = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "prelude-extras";
          version = "0.4.0.3";
          sha256 = "09bb087f0870a353ec1e7e1a08017b9a766d430d956afb88ca000a6a876bf877";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/prelude-extras";
          description = "Higher order versions of Prelude classes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pretty = callPackage ({ base, deepseq, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "pretty";
          version = "1.1.3.3";
          sha256 = "3b632679f51cc709ec96e51c6a03bbc1ded8dbc5c8ea3fda75501cf7962f9798";
          libraryHaskellDepends = [
            base
            deepseq
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/haskell/pretty";
          description = "Pretty-printing library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      primitive = callPackage ({ base, ghc-prim, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "primitive";
          version = "0.6.2.0";
          sha256 = "b8e8d70213e22b3fab0e0d11525c02627489618988fdc636052ca0adce282ae1";
          revision = "1";
          editedCabalFile = "0d61g8ppsdajdqykl2kc46kq00aamsf12v60ilgrf58dbji9sz56";
          libraryHaskellDepends = [
            base
            ghc-prim
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/primitive";
          description = "Primitive memory-related operations";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      process = callPackage ({ base, deepseq, directory, filepath, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "process";
          version = "1.4.3.0";
          sha256 = "5473f4d20a19c3ba448ace7d4d01ec821ad531574c23934fd3c55627f5a7f0eb";
          libraryHaskellDepends = [
            base
            deepseq
            directory
            filepath
            unix
          ];
          doHaddock = false;
          doCheck = false;
          description = "Process libraries";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      profunctors = callPackage ({ base, base-orphans, bifunctors, comonad, contravariant, distributive, mkDerivation, stdenv, tagged, transformers }:
      mkDerivation {
          pname = "profunctors";
          version = "5.2.1";
          sha256 = "e7207e00dfa6f36d9f84568b1aa4b3bd2077f5fced387cd6cf75b411d0959c5d";
          libraryHaskellDepends = [
            base
            base-orphans
            bifunctors
            comonad
            contravariant
            distributive
            tagged
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/profunctors/";
          description = "Profunctors";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      psqueues = callPackage ({ base, deepseq, ghc-prim, hashable, mkDerivation, stdenv }:
      mkDerivation {
          pname = "psqueues";
          version = "0.2.3.0";
          sha256 = "25ed95de5de62831d3e1db47aabd0b589ddf71aeec2d53bc3104a9bb663743a7";
          libraryHaskellDepends = [
            base
            deepseq
            ghc-prim
            hashable
          ];
          doHaddock = false;
          doCheck = false;
          description = "Pure priority search queues";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      purescript-bridge = callPackage ({ base, containers, directory, filepath, generic-deriving, lens, mkDerivation, mtl, stdenv, text, transformers }:
      mkDerivation {
          pname = "purescript-bridge";
          version = "0.11.0.0";
          sha256 = "6e4f2ea1d550d1d3ee4ede18a18d95aa37c587c2aef54d4aee22306550a45878";
          libraryHaskellDepends = [
            base
            containers
            directory
            filepath
            generic-deriving
            lens
            mtl
            text
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          description = "Generate PureScript data types from Haskell data types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pvss = callPackage ({ base, binary, bytestring, cryptonite, cryptonite-openssl, deepseq, foundation, hourglass, integer-gmp, memory, mkDerivation, stdenv, vector }:
      mkDerivation {
          pname = "pvss";
          version = "0.2.0";
          sha256 = "e440145003cac581a43941e82b213011cb0730c524948e9aaec9d3376622028c";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            binary
            bytestring
            cryptonite
            cryptonite-openssl
            deepseq
            foundation
            integer-gmp
            memory
          ];
          executableHaskellDepends = [
            base
            cryptonite
            deepseq
            hourglass
            memory
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/input-output-hk/pvss-haskell#readme";
          description = "Public Verifiable Secret Sharing";
          license = stdenv.lib.licenses.mit;
        }) {};
      quickcheck-instances = callPackage ({ QuickCheck, array, base, bytestring, containers, hashable, mkDerivation, old-time, scientific, stdenv, text, time, unordered-containers, vector }:
      mkDerivation {
          pname = "quickcheck-instances";
          version = "0.3.12";
          sha256 = "ddd5b988da50eff7f56737bff516fba52309f7461297698f04f1e8aaee9f9bf3";
          revision = "2";
          editedCabalFile = "1v1r7gidkjc2v4dw1id57raqnjqv4rc10pa2l6xhhg0dzrnw28a3";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
            hashable
            old-time
            QuickCheck
            scientific
            text
            time
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/aslatter/qc-instances";
          description = "Common quickcheck instances";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      quickcheck-io = callPackage ({ HUnit, QuickCheck, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "quickcheck-io";
          version = "0.2.0";
          sha256 = "fb779119d79fe08ff4d502fb6869a70c9a8d5fd8ae0959f605c3c937efd96422";
          libraryHaskellDepends = [
            base
            HUnit
            QuickCheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hspec/quickcheck-io#readme";
          description = "Use HUnit assertions as QuickCheck properties";
          license = stdenv.lib.licenses.mit;
        }) {};
      random = callPackage ({ base, mkDerivation, stdenv, time }:
      mkDerivation {
          pname = "random";
          version = "1.1";
          sha256 = "b718a41057e25a3a71df693ab0fe2263d492e759679b3c2fea6ea33b171d3a5a";
          revision = "1";
          editedCabalFile = "1pv5d7bm2rgap7llp5vjsplrg048gvf0226y0v19gpvdsx7n4rvv";
          libraryHaskellDepends = [
            base
            time
          ];
          doHaddock = false;
          doCheck = false;
          description = "random number library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      random-shuffle = callPackage ({ MonadRandom, base, mkDerivation, random, stdenv }:
      mkDerivation {
          pname = "random-shuffle";
          version = "0.0.4";
          sha256 = "52704411f040fd0bf2361dad162e35dc13caa6535b2e4908d3513c00a95d0615";
          libraryHaskellDepends = [
            base
            MonadRandom
            random
          ];
          doHaddock = false;
          doCheck = false;
          description = "Random shuffle implementation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      reflection = callPackage ({ base, mkDerivation, stdenv, template-haskell }:
      mkDerivation {
          pname = "reflection";
          version = "2.1.2";
          sha256 = "a909882c04b24016bedb85587c09f23cf06bad71a2b1f7e781e89abaa6023c39";
          libraryHaskellDepends = [
            base
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/reflection";
          description = "Reifies arbitrary terms into types that can be reflected back into terms";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      regex-base = callPackage ({ array, base, bytestring, containers, mkDerivation, mtl, stdenv }:
      mkDerivation {
          pname = "regex-base";
          version = "0.93.2";
          sha256 = "20dc5713a16f3d5e2e6d056b4beb9cfdc4368cd09fd56f47414c847705243278";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
            mtl
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://sourceforge.net/projects/lazy-regex";
          description = "Replaces/Enhances Text.Regex";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      regex-tdfa = callPackage ({ array, base, bytestring, containers, ghc-prim, mkDerivation, mtl, parsec, regex-base, stdenv }:
      mkDerivation {
          pname = "regex-tdfa";
          version = "1.2.2";
          sha256 = "cb12d675be7b31ed8086d8d022023d03eb553e55dbee6e1b7a4154933d471d39";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
            ghc-prim
            mtl
            parsec
            regex-base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ChrisKuklewicz/regex-tdfa";
          description = "Replaces/Enhances Text.Regex";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      regex-tdfa-text = callPackage ({ array, base, mkDerivation, regex-base, regex-tdfa, stdenv, text }:
      mkDerivation {
          pname = "regex-tdfa-text";
          version = "1.0.0.3";
          sha256 = "38d77a0d225c306c52c6d4eed12d11d05a4bc4194d547cb9a7a9b6f5a8792001";
          libraryHaskellDepends = [
            array
            base
            regex-base
            regex-tdfa
            text
          ];
          doHaddock = false;
          doCheck = false;
          description = "Text interface for regex-tdfa";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      resourcet = callPackage ({ base, containers, exceptions, lifted-base, mkDerivation, mmorph, monad-control, mtl, stdenv, transformers, transformers-base, transformers-compat }:
      mkDerivation {
          pname = "resourcet";
          version = "1.1.9";
          sha256 = "5a1999d26b896603cab8121b77f36723dc50960291872b691ff4a9533e162ef5";
          libraryHaskellDepends = [
            base
            containers
            exceptions
            lifted-base
            mmorph
            monad-control
            mtl
            transformers
            transformers-base
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/snoyberg/conduit";
          description = "Deterministic allocation and freeing of scarce resources";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      rocksdb-haskell = callPackage ({ base, binary, bytestring, data-default, filepath, mkDerivation, resourcet, rocksdb, stdenv, transformers }:
      mkDerivation {
          pname = "rocksdb-haskell";
          version = "1.0.0";
          sha256 = "eddbc713b2203787c2218c40989bf244b216105ac528e9738204aaca15bf5165";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            data-default
            filepath
            resourcet
            transformers
          ];
          librarySystemDepends = [
            rocksdb
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/serokell/rocksdb-haskell";
          description = "Haskell bindings to RocksDB";
          license = stdenv.lib.licenses.bsd3;
        }) { rocksdb = pkgs.rocksdb; };
      safe = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "safe";
          version = "0.3.15";
          sha256 = "a35e4ae609aabd568da7e7d220ab529c34040b71ae50df1ee353896445a66a2d";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ndmitchell/safe#readme";
          description = "Library of safe (exception free) functions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      safe-exceptions = callPackage ({ base, deepseq, exceptions, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "safe-exceptions";
          version = "0.1.6.0";
          sha256 = "71d47ce1049465b02d89231f2931e7a1d22b6960e85fca5281162e979cf08d1c";
          libraryHaskellDepends = [
            base
            deepseq
            exceptions
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fpco/safe-exceptions#readme";
          description = "Safe, consistent, and easy exception handling";
          license = stdenv.lib.licenses.mit;
        }) {};
      safecopy = callPackage ({ array, base, bytestring, cereal, containers, mkDerivation, old-time, semigroups, stdenv, template-haskell, text, time, vector }:
      mkDerivation {
          pname = "safecopy";
          version = "0.9.3.2";
          sha256 = "05f5508d8e6db1f71056096373e5123586fdd704f3765cc9857f1bffb2e46b37";
          libraryHaskellDepends = [
            array
            base
            bytestring
            cereal
            containers
            old-time
            semigroups
            template-haskell
            text
            time
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/acid-state/safecopy";
          description = "Binary serialization with version control";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      scientific = callPackage ({ base, binary, bytestring, containers, deepseq, ghc-prim, hashable, integer-gmp, integer-logarithms, mkDerivation, primitive, stdenv, text }:
      mkDerivation {
          pname = "scientific";
          version = "0.3.5.1";
          sha256 = "5ba6e682cbb6cd5c6444d1c6c35f3b396e316637a14456b18de58c9a33661ba6";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            containers
            deepseq
            ghc-prim
            hashable
            integer-gmp
            integer-logarithms
            primitive
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/scientific";
          description = "Numbers represented using scientific notation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      scrypt = callPackage ({ base, base64-bytestring, bytestring, entropy, mkDerivation, stdenv }:
      mkDerivation {
          pname = "scrypt";
          version = "0.5.0";
          sha256 = "3ec0a622393e2a4dbbce4c899602c848d924f8516688491b1162331b7093d9b2";
          libraryHaskellDepends = [
            base
            base64-bytestring
            bytestring
            entropy
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/informatikr/scrypt";
          description = "Stronger password hashing via sequential memory-hard functions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      semigroupoids = callPackage ({ Cabal, base, base-orphans, bifunctors, cabal-doctest, comonad, containers, contravariant, distributive, hashable, mkDerivation, semigroups, stdenv, tagged, transformers, transformers-compat, unordered-containers }:
      mkDerivation {
          pname = "semigroupoids";
          version = "5.2.1";
          sha256 = "79e41eb7cbcb4f152343b91243feac0a120375284c1207edaa73b23d8df6d200";
          revision = "3";
          editedCabalFile = "0wzcnpz8pyjk823vqnq5s8krsb8i6cw573hcschpd9x5ynq4li70";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            base
            base-orphans
            bifunctors
            comonad
            containers
            contravariant
            distributive
            hashable
            semigroups
            tagged
            transformers
            transformers-compat
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/semigroupoids";
          description = "Semigroupoids: Category sans id";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      semigroups = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "semigroups";
          version = "0.18.3";
          sha256 = "35297c986872406e2efe29620c623727369f8c578e2f9c22998d575996e5a9ca";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/semigroups/";
          description = "Anything that associates";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      semver = callPackage ({ attoparsec, base, deepseq, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "semver";
          version = "0.3.3.1";
          sha256 = "36d3369706836d60f3bc517f30c6860734481866363723904b8768823b6bc8b1";
          libraryHaskellDepends = [
            attoparsec
            base
            deepseq
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/brendanhay/semver";
          description = "Representation, manipulation, and de/serialisation of Semantic Versions";
          license = "unknown";
        }) {};
      serokell-util = callPackage ({ QuickCheck, acid-state, aeson, ansi-terminal, base, base16-bytestring, base64-bytestring, bytestring, clock, containers, deepseq, directory, exceptions, extra, filepath, formatting, hashable, lens, log-warper, mkDerivation, monad-control, mtl, optparse-applicative, parsec, quickcheck-instances, safecopy, scientific, semigroups, stdenv, stm, template-haskell, text, text-format, time-units, transformers, universum, unordered-containers, vector, yaml }:
      mkDerivation {
          pname = "serokell-util";
          version = "0.5.0";
          sha256 = "3ce6dfd005ebc2d64f506eebed1562e5368c006f9c9e0576f0e4c0308b04867c";
          libraryHaskellDepends = [
            acid-state
            aeson
            ansi-terminal
            base
            base16-bytestring
            base64-bytestring
            bytestring
            clock
            containers
            deepseq
            directory
            exceptions
            extra
            filepath
            formatting
            hashable
            lens
            log-warper
            monad-control
            mtl
            optparse-applicative
            parsec
            QuickCheck
            quickcheck-instances
            safecopy
            scientific
            semigroups
            stm
            template-haskell
            text
            text-format
            time-units
            transformers
            universum
            unordered-containers
            vector
            yaml
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/serokell/serokell-util";
          description = "General-purpose functions by Serokell";
          license = stdenv.lib.licenses.mit;
        }) {};
      servant = callPackage ({ Cabal, aeson, attoparsec, base, base-compat, bytestring, case-insensitive, directory, filepath, http-api-data, http-media, http-types, mkDerivation, mmorph, mtl, natural-transformation, network-uri, stdenv, string-conversions, text, vault }:
      mkDerivation {
          pname = "servant";
          version = "0.10";
          sha256 = "e1daa9ba2b759615341345a17a95833729ae3200af12dacec07507a95a4b331e";
          revision = "3";
          editedCabalFile = "105fvx77sgx23q52spm1r1xchwbmvxc45hhjccasx68kpwbhdgy7";
          setupHaskellDepends = [
            base
            Cabal
            directory
            filepath
          ];
          libraryHaskellDepends = [
            aeson
            attoparsec
            base
            base-compat
            bytestring
            case-insensitive
            http-api-data
            http-media
            http-types
            mmorph
            mtl
            natural-transformation
            network-uri
            string-conversions
            text
            vault
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-servant.readthedocs.org/";
          description = "A family of combinators for defining webservices APIs";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      servant-blaze = callPackage ({ base, blaze-html, http-media, mkDerivation, servant, stdenv }:
      mkDerivation {
          pname = "servant-blaze";
          version = "0.7.1";
          sha256 = "90ed1c7a22b83bee344ef3896203f3699b7633bf986ffa064752c3596c072646";
          revision = "6";
          editedCabalFile = "051m44rqmxkl30n96qcbz1xwwsw2n7l7laflnc0xydc40ws0bj96";
          libraryHaskellDepends = [
            base
            blaze-html
            http-media
            servant
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-servant.readthedocs.org/";
          description = "Blaze-html support for servant";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      servant-multipart = callPackage ({ base, bytestring, directory, http-client, http-media, mkDerivation, network, resourcet, servant, servant-server, stdenv, text, transformers, wai, wai-extra, warp }:
      mkDerivation {
          pname = "servant-multipart";
          version = "0.10";
          sha256 = "285298e7411297f4e023cceba14fd8f5c03c694f47533adfaf01538682698199";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            bytestring
            directory
            http-media
            resourcet
            servant
            servant-server
            text
            transformers
            wai
            wai-extra
          ];
          executableHaskellDepends = [
            base
            http-client
            network
            servant
            servant-server
            text
            transformers
            wai
            warp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-servant/servant-multipart#readme";
          description = "multipart/form-data (e.g file upload) support for servant";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      servant-server = callPackage ({ Cabal, aeson, attoparsec, base, base-compat, base64-bytestring, bytestring, containers, directory, exceptions, filepath, http-api-data, http-types, mkDerivation, monad-control, mtl, network, network-uri, resourcet, safe, servant, split, stdenv, string-conversions, system-filepath, text, transformers, transformers-base, transformers-compat, wai, wai-app-static, warp, word8 }:
      mkDerivation {
          pname = "servant-server";
          version = "0.10";
          sha256 = "99d14d23ea67832401b4bca7e5cb75b8c557e6dc7a8f38870c3b9d701179073d";
          revision = "2";
          editedCabalFile = "06kbdp3c7mcbmr92z1qbq51x2mr3d98vhi52p4y87ggwkmc5w5rr";
          isLibrary = true;
          isExecutable = true;
          setupHaskellDepends = [
            base
            Cabal
            directory
            filepath
          ];
          libraryHaskellDepends = [
            aeson
            attoparsec
            base
            base-compat
            base64-bytestring
            bytestring
            containers
            exceptions
            filepath
            http-api-data
            http-types
            monad-control
            mtl
            network
            network-uri
            resourcet
            safe
            servant
            split
            string-conversions
            system-filepath
            text
            transformers
            transformers-base
            transformers-compat
            wai
            wai-app-static
            warp
            word8
          ];
          executableHaskellDepends = [
            aeson
            base
            servant
            text
            wai
            warp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-servant.readthedocs.org/";
          description = "A family of combinators for defining webservices APIs and serving them";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      servant-swagger = callPackage ({ Cabal, QuickCheck, aeson, base, bytestring, cabal-doctest, hspec, http-media, insert-ordered-containers, lens, mkDerivation, servant, stdenv, swagger2, text, unordered-containers }:
      mkDerivation {
          pname = "servant-swagger";
          version = "1.1.3.1";
          sha256 = "e8d85d05f4251b7bdbd7c5f215d90a22eb55a46812bc82469d94d2f07adebb58";
          revision = "1";
          editedCabalFile = "1bx68rcz4whjw3pqm40aiqpfigcgg9dkgjdlggry2iv81s0415xf";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            aeson
            base
            bytestring
            hspec
            http-media
            insert-ordered-containers
            lens
            QuickCheck
            servant
            swagger2
            text
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-servant/servant-swagger";
          description = "Generate Swagger specification for your servant API";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      servant-swagger-ui = callPackage ({ base, blaze-markup, bytestring, directory, file-embed, filepath, http-media, mkDerivation, servant, servant-blaze, servant-server, servant-swagger, stdenv, swagger2, template-haskell, text, transformers, transformers-compat, wai-app-static }:
      mkDerivation {
          pname = "servant-swagger-ui";
          version = "0.2.4.3.0.20";
          sha256 = "b603d7da9141714a5eab226d015ffe566294671840c84d9bf94c4ea0114817a3";
          revision = "1";
          editedCabalFile = "1wsbb9zaq5qv39hrymy1cma581337rbvqlm7y24jwfvk4vafs3fp";
          libraryHaskellDepends = [
            base
            blaze-markup
            bytestring
            directory
            file-embed
            filepath
            http-media
            servant
            servant-blaze
            servant-server
            servant-swagger
            swagger2
            template-haskell
            text
            transformers
            transformers-compat
            wai-app-static
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/servant-swagger-ui#readme";
          description = "Servant swagger ui";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      setenv = callPackage ({ base, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "setenv";
          version = "0.1.1.3";
          sha256 = "e358df39afc03d5a39e2ec650652d845c85c80cc98fe331654deafb4767ecb32";
          revision = "1";
          editedCabalFile = "0ny4g3kjys0hqg41mnwrsymy1bwhl8l169kis4y4fa58sb06m4f5";
          libraryHaskellDepends = [
            base
            unix
          ];
          doHaddock = false;
          doCheck = false;
          description = "A cross-platform library for setting environment variables";
          license = stdenv.lib.licenses.mit;
        }) {};
      silently = callPackage ({ base, deepseq, directory, mkDerivation, stdenv }:
      mkDerivation {
          pname = "silently";
          version = "1.2.5";
          sha256 = "cef625635053a46032ca53b43d311921875a437910b6568ded17027fdca83839";
          libraryHaskellDepends = [
            base
            deepseq
            directory
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hspec/silently";
          description = "Prevent or capture writing to stdout and other handles";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      simple-sendfile = callPackage ({ base, bytestring, mkDerivation, network, stdenv, unix }:
      mkDerivation {
          pname = "simple-sendfile";
          version = "0.2.25";
          sha256 = "0ae68821cd828b29772654b5613d514a421b1b1440d82a4b610339e67a92294d";
          revision = "1";
          editedCabalFile = "1axghvn2iz0gzlc0ics4q8abl15ggwvcwcmly5cxhmc32hqv8y5c";
          libraryHaskellDepends = [
            base
            bytestring
            network
            unix
          ];
          doHaddock = false;
          doCheck = false;
          description = "Cross platform library for the sendfile system call";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      socket-io = callPackage ({ aeson, attoparsec, base, bytestring, engine-io, fetchgit, mkDerivation, mtl, stdenv, stm, text, transformers, unordered-containers, vector }:
      mkDerivation {
          pname = "socket-io";
          version = "1.3.7";
          src = fetchgit {
            url = "https://github.com/serokell/engine.io.git";
            sha256 = "0j2rxbw5g88ivmjzhmhnxk4cgkxdw97i2qlzw47gzyv56ciqfdny";
            rev = "a594e402fd450f11ad60d09ddbd93db500000632";
          };
          postUnpack = "sourceRoot+=/socket-io; echo source root reset to \$sourceRoot";
          libraryHaskellDepends = [
            aeson
            attoparsec
            base
            bytestring
            engine-io
            mtl
            stm
            text
            transformers
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ocharles/engine.io";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      socks = callPackage ({ base, bytestring, cereal, mkDerivation, network, stdenv }:
      mkDerivation {
          pname = "socks";
          version = "0.5.5";
          sha256 = "2647ea93e21ad1dfd77e942c022c8707e468d25e1ff672a88be82508034fc868";
          revision = "1";
          editedCabalFile = "0nz8q0xvd8y6f42bd1w3q8d8bg1qzl8ggx0a23kb3jb60g36dmvw";
          libraryHaskellDepends = [
            base
            bytestring
            cereal
            network
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-socks";
          description = "Socks proxy (version 5) implementation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      split = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "split";
          version = "0.2.3.2";
          sha256 = "4943eaad0dd473d44b4b97b8b9731c20f05ba86abb8a1fa07f8df819f09eb63a";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Combinator library for splitting lists";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      statestack = callPackage ({ base, mkDerivation, mtl, stdenv, transformers, transformers-compat }:
      mkDerivation {
          pname = "statestack";
          version = "0.2.0.5";
          sha256 = "f4eadcf9b08c14cb084436f81e16edf78d6eeda77a3f93e38ba5d7e263ea5f66";
          revision = "1";
          editedCabalFile = "0kf1jdhdv9fiwlbn2915sg39x23lfxlyp2qb7jkrvx8p8v2sam7i";
          libraryHaskellDepends = [
            base
            mtl
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          description = "Simple State-like monad transformer with saveable and restorable state";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      statistics = callPackage ({ aeson, base, binary, deepseq, erf, math-functions, mkDerivation, monad-par, mwc-random, primitive, stdenv, vector, vector-algorithms, vector-binary-instances }:
      mkDerivation {
          pname = "statistics";
          version = "0.13.3.0";
          sha256 = "6e7fe0f10086725c696fdd855caf4b6fb58ca5100bd0c9995f575f5b071381ed";
          libraryHaskellDepends = [
            aeson
            base
            binary
            deepseq
            erf
            math-functions
            monad-par
            mwc-random
            primitive
            vector
            vector-algorithms
            vector-binary-instances
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/statistics";
          description = "A library of statistical types, data, and functions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      stm = callPackage ({ array, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "stm";
          version = "2.4.4.1";
          sha256 = "8f999095ed8d50d2056fc6e185035ee8166c50751e1af8de02ac38d382bf3384";
          revision = "1";
          editedCabalFile = "0kzw4rw9fgmc4qyxmm1lwifdyrx5r1356150xm14vy4mp86diks9";
          libraryHaskellDepends = [
            array
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Software Transactional Memory";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      stm-containers = callPackage ({ base, base-prelude, focus, hashable, list-t, mkDerivation, primitive, stdenv, transformers }:
      mkDerivation {
          pname = "stm-containers";
          version = "0.2.16";
          sha256 = "69042f06647cdc69e1ecf83863d88d67acd377f631d8a15966df67245152502f";
          libraryHaskellDepends = [
            base
            base-prelude
            focus
            hashable
            list-t
            primitive
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nikita-volkov/stm-containers";
          description = "Containers for STM";
          license = stdenv.lib.licenses.mit;
        }) {};
      stm-delay = callPackage ({ base, mkDerivation, stdenv, stm }:
      mkDerivation {
          pname = "stm-delay";
          version = "0.1.1.1";
          sha256 = "b132581aac47e6cba6a1691a485e1700fbb047c02b7e1e43ae9bbd8476108a32";
          libraryHaskellDepends = [
            base
            stm
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/joeyadams/haskell-stm-delay";
          description = "Updatable one-shot timer polled with STM";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      streaming-commons = callPackage ({ array, async, base, blaze-builder, bytestring, directory, mkDerivation, network, process, random, stdenv, stm, text, transformers, unix, zlib }:
      mkDerivation {
          pname = "streaming-commons";
          version = "0.1.17";
          sha256 = "e50a38cb8b626ef2f031c195e22171ffce00e20cbe63e8c768887564a7f47da9";
          libraryHaskellDepends = [
            array
            async
            base
            blaze-builder
            bytestring
            directory
            network
            process
            random
            stm
            text
            transformers
            unix
            zlib
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fpco/streaming-commons";
          description = "Common lower-level functions needed by various streaming data libraries";
          license = stdenv.lib.licenses.mit;
        }) {};
      string-conversions = callPackage ({ base, bytestring, mkDerivation, stdenv, text, utf8-string }:
      mkDerivation {
          pname = "string-conversions";
          version = "0.4.0.1";
          sha256 = "46bcce6d9ce62c558b7658a75d9c6a62f7259d6b0473d011d8078234ad6a1994";
          libraryHaskellDepends = [
            base
            bytestring
            text
            utf8-string
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/soenkehahn/string-conversions#readme";
          description = "Simplifies dealing with different types for strings";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      string-qq = callPackage ({ base, mkDerivation, stdenv, template-haskell }:
      mkDerivation {
          pname = "string-qq";
          version = "0.0.2";
          sha256 = "9757cad387856a313729caffe0639215a10be7d72b09c44bcab9e55ee2a8c218";
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            base
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          description = "QuasiQuoter for non-interpolated strings, texts and bytestrings";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      stringbuilder = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "stringbuilder";
          version = "0.5.0";
          sha256 = "8966882622fc06fd4e588da626a558b54daa313f2328c188d9305b0c6f2fe9aa";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "A writer monad for multi-line string literals";
          license = stdenv.lib.licenses.mit;
        }) {};
      stringsearch = callPackage ({ array, base, bytestring, containers, mkDerivation, stdenv }:
      mkDerivation {
          pname = "stringsearch";
          version = "0.3.6.6";
          sha256 = "295f1971920bc52263d8275d7054ad223a7e1aefe75533f9887735c9644ffe4a";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://bitbucket.org/dafis/stringsearch";
          description = "Fast searching, splitting and replacing of ByteStrings";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      svg-builder = callPackage ({ base, blaze-builder, bytestring, hashable, mkDerivation, stdenv, text, unordered-containers }:
      mkDerivation {
          pname = "svg-builder";
          version = "0.1.0.2";
          sha256 = "81490cf0c843d6d7795ba32ac6cb05acf4a92431fe7702aa634ec52d60bfee54";
          revision = "1";
          editedCabalFile = "1h3bzkimiydj5j2rh7cyp5bhphvy6hglpkidhlfwy520sqsw3zvx";
          libraryHaskellDepends = [
            base
            blaze-builder
            bytestring
            hashable
            text
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/jeffreyrosenbluth/svg-builder.git";
          description = "DSL for building SVG";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      swagger2 = callPackage ({ Cabal, aeson, base, base-compat, bytestring, cabal-doctest, containers, fetchgit, generics-sop, hashable, http-media, insert-ordered-containers, lens, mkDerivation, mtl, network, scientific, stdenv, template-haskell, text, time, transformers, transformers-compat, unordered-containers, uuid-types, vector }:
      mkDerivation {
          pname = "swagger2";
          version = "2.1.4.1";
          src = fetchgit {
            url = "https://github.com/serokell/swagger2";
            sha256 = "1pavnbzpx5ybdwc7ridjz3mljmcwjlakrr7mpdkgl2dliyrrwj6d";
            rev = "6693ff91be2f0b015c2ab043292587c1cc9449cf";
          };
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            aeson
            base
            base-compat
            bytestring
            containers
            generics-sop
            hashable
            http-media
            insert-ordered-containers
            lens
            mtl
            network
            scientific
            template-haskell
            text
            time
            transformers
            transformers-compat
            unordered-containers
            uuid-types
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/GetShopTV/swagger2";
          description = "Swagger 2.0 data model";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      syb = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "syb";
          version = "0.7";
          sha256 = "b8757dce5ab4045c49a0ae90407d575b87ee5523a7dd5dfa5c9d54fcceff42b5";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.cs.uu.nl/wiki/GenericProgramming/SYB";
          description = "Scrap Your Boilerplate";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      system-filepath = callPackage ({ base, bytestring, deepseq, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "system-filepath";
          version = "0.4.13.4";
          sha256 = "345d7dec968b74ab1b8c0e7bb78c2ef1e5be7be6b7bac455340fd658abfec5fb";
          libraryHaskellDepends = [
            base
            bytestring
            deepseq
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fpco/haskell-filesystem";
          description = "High-level, byte-based file and directory path manipulations (deprecated)";
          license = stdenv.lib.licenses.mit;
        }) {};
      systemd = callPackage ({ base, bytestring, mkDerivation, network, stdenv, transformers, unix }:
      mkDerivation {
          pname = "systemd";
          version = "1.1.2";
          sha256 = "59461920b66b4b63b055b08af464a6fd9ff529f64527dfb573f9396dadd39287";
          libraryHaskellDepends = [
            base
            bytestring
            network
            transformers
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/erebe/systemd";
          description = "Systemd facilities (Socket activation, Notify)";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      tagged = callPackage ({ base, deepseq, mkDerivation, stdenv, template-haskell, transformers, transformers-compat }:
      mkDerivation {
          pname = "tagged";
          version = "0.8.5";
          sha256 = "e47c51c955ed77b0fa36897f652df990aa0a8c4eb278efaddcd604be00fc8d99";
          revision = "1";
          editedCabalFile = "15mqdimbgrq5brqljjl7dbxkyrxppap06q53cp7ml7w3l08v5mx8";
          libraryHaskellDepends = [
            base
            deepseq
            template-haskell
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/tagged";
          description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      tar = callPackage ({ array, base, bytestring, containers, deepseq, directory, filepath, mkDerivation, stdenv, time }:
      mkDerivation {
          pname = "tar";
          version = "0.5.0.3";
          sha256 = "d8d9ad876365f88bdccd02073049e58715cd5ba94de06eb98e21d595244918a3";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
            deepseq
            directory
            filepath
            time
          ];
          doHaddock = false;
          doCheck = false;
          description = "Reading, writing and manipulating \".tar\" archive files.";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      template-haskell = callPackage ({ base, ghc-boot-th, mkDerivation, pretty, stdenv }:
      mkDerivation {
          pname = "template-haskell";
          version = "2.11.1.0";
          sha256 = "5fb340b665fad764238a67b6dd04870a8c4b15e891a8d2d2cd37c5915a7b369c";
          libraryHaskellDepends = [
            base
            ghc-boot-th
            pretty
          ];
          doHaddock = false;
          doCheck = false;
          description = "Support library for Template Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      temporary = callPackage ({ base, directory, exceptions, filepath, mkDerivation, stdenv, transformers, unix }:
      mkDerivation {
          pname = "temporary";
          version = "1.2.1.1";
          sha256 = "55772471e59529f4bde9555f6abb21d19a611c7d70b13befe114dc1a0ecb00f3";
          libraryHaskellDepends = [
            base
            directory
            exceptions
            filepath
            transformers
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/feuerbach/temporary";
          description = "Portable temporary file and directory support";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      text = callPackage ({ array, base, binary, bytestring, deepseq, ghc-prim, integer-gmp, mkDerivation, stdenv }:
      mkDerivation {
          pname = "text";
          version = "1.2.2.2";
          sha256 = "31465106360a7d7e214d96f1d1b4303a113ffce1bde44a4e614053a1e5072df9";
          libraryHaskellDepends = [
            array
            base
            binary
            bytestring
            deepseq
            ghc-prim
            integer-gmp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/text";
          description = "An efficient packed Unicode text type";
          license = stdenv.lib.licenses.bsd2;
        }) {};
      text-format = callPackage ({ array, base, double-conversion, ghc-prim, integer-gmp, mkDerivation, old-locale, stdenv, text, time, transformers }:
      mkDerivation {
          pname = "text-format";
          version = "0.3.1.1";
          sha256 = "6de112764446a65370204f35a5fc4b1831106049f90918545d5dcd2ddd7fee0b";
          libraryHaskellDepends = [
            array
            base
            double-conversion
            ghc-prim
            integer-gmp
            old-locale
            text
            time
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/text-format";
          description = "Text formatting";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      tf-random = callPackage ({ base, mkDerivation, primitive, random, stdenv, time }:
      mkDerivation {
          pname = "tf-random";
          version = "0.5";
          sha256 = "2e30cec027b313c9e1794d326635d8fc5f79b6bf6e7580ab4b00186dadc88510";
          libraryHaskellDepends = [
            base
            primitive
            random
            time
          ];
          doHaddock = false;
          doCheck = false;
          description = "High-quality splittable pseudorandom number generator";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      th-abstraction = callPackage ({ base, containers, ghc-prim, mkDerivation, stdenv, template-haskell }:
      mkDerivation {
          pname = "th-abstraction";
          version = "0.2.5.0";
          sha256 = "77ea921843efdd26fe44b1a42574d635b31b4d1c69234c84f6fe156f97bdaabb";
          libraryHaskellDepends = [
            base
            containers
            ghc-prim
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/glguy/th-abstraction";
          description = "Nicer interface for reified information about data types";
          license = stdenv.lib.licenses.isc;
        }) {};
      th-expand-syns = callPackage ({ base, containers, mkDerivation, stdenv, syb, template-haskell }:
      mkDerivation {
          pname = "th-expand-syns";
          version = "0.4.3.0";
          sha256 = "9fee68a387610574ed6445022fdcd0879a7415d910dcb6618f1de5d2001e679d";
          libraryHaskellDepends = [
            base
            containers
            syb
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/DanielSchuessler/th-expand-syns";
          description = "Expands type synonyms in Template Haskell ASTs";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      th-lift = callPackage ({ base, ghc-prim, mkDerivation, stdenv, template-haskell }:
      mkDerivation {
          pname = "th-lift";
          version = "0.7.7";
          sha256 = "16c6fa6fbe972fa0d850698c147cd9a30dc0e201554d9a4ee9ade62dc807cbb5";
          libraryHaskellDepends = [
            base
            ghc-prim
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/mboes/th-lift";
          description = "Derive Template Haskell's Lift class for datatypes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      th-lift-instances = callPackage ({ base, bytestring, containers, mkDerivation, stdenv, template-haskell, text, th-lift, vector }:
      mkDerivation {
          pname = "th-lift-instances";
          version = "0.1.11";
          sha256 = "1da46afabdc73c86f279a0557d5a8f9af1296f9f6043264ba354b1c9cc65a6b8";
          libraryHaskellDepends = [
            base
            bytestring
            containers
            template-haskell
            text
            th-lift
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/bennofs/th-lift-instances/";
          description = "Lift instances for template-haskell for common data types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      th-orphans = callPackage ({ base, mkDerivation, mtl, stdenv, template-haskell, th-lift, th-lift-instances, th-reify-many }:
      mkDerivation {
          pname = "th-orphans";
          version = "0.13.4";
          sha256 = "f395d9efa0ed105659cdcc8a1b89ae9db62f4bd3a042794ab882c4e82b344b31";
          libraryHaskellDepends = [
            base
            mtl
            template-haskell
            th-lift
            th-lift-instances
            th-reify-many
          ];
          doHaddock = false;
          doCheck = false;
          description = "Orphan instances for TH datatypes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      th-reify-many = callPackage ({ base, containers, mkDerivation, mtl, safe, stdenv, template-haskell, th-expand-syns }:
      mkDerivation {
          pname = "th-reify-many";
          version = "0.1.8";
          sha256 = "cecaae187df911de515d08929e1394d6d6f7ce129795be8189a6b10d3734fe43";
          libraryHaskellDepends = [
            base
            containers
            mtl
            safe
            template-haskell
            th-expand-syns
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/mgsloan/th-reify-many";
          description = "Recurseively reify template haskell datatype info";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      th-utilities = callPackage ({ base, bytestring, containers, directory, filepath, mkDerivation, primitive, stdenv, syb, template-haskell, text, th-orphans }:
      mkDerivation {
          pname = "th-utilities";
          version = "0.2.0.1";
          sha256 = "65c64cee69c0d9bf8d0d5d4590aaea7dcf4177f97818526cbb3fac20901671d6";
          libraryHaskellDepends = [
            base
            bytestring
            containers
            directory
            filepath
            primitive
            syb
            template-haskell
            text
            th-orphans
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fpco/th-utilities#readme";
          description = "Collection of useful functions for use with Template Haskell";
          license = stdenv.lib.licenses.mit;
        }) {};
      time = callPackage ({ base, deepseq, mkDerivation, stdenv }:
      mkDerivation {
          pname = "time";
          version = "1.6.0.1";
          sha256 = "ff69b46f38f4d226b171d078b200f8a5a1e8cfeadfa543eabade51355d7c7fcb";
          libraryHaskellDepends = [
            base
            deepseq
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/time";
          description = "A time library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      time-locale-compat = callPackage ({ base, mkDerivation, old-locale, stdenv, time }:
      mkDerivation {
          pname = "time-locale-compat";
          version = "0.1.1.3";
          sha256 = "9144bf68b47791a2ac73f45aeadbc5910be2da9ad174909e1a10a70b4576aced";
          libraryHaskellDepends = [
            base
            old-locale
            time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/khibino/haskell-time-locale-compat";
          description = "Compatibility of TimeLocale between old-locale and time-1.5";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      time-units = callPackage ({ base, fetchgit, mkDerivation, stdenv }:
      mkDerivation {
          pname = "time-units";
          version = "1.0.0";
          src = fetchgit {
            url = "https://github.com/serokell/time-units.git";
            sha256 = "0psdr1if0rgnn24698x3583m0603rwd3sd7yb9whj03hskmkwpgs";
            rev = "6c3747c1ac794f952de996dd7ba8a2f6d63bf132";
          };
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/acw/time-units";
          description = "A basic library for defining units of time as types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      tls = callPackage ({ asn1-encoding, asn1-types, async, base, bytestring, cereal, cryptonite, data-default-class, memory, mkDerivation, mtl, network, stdenv, transformers, x509, x509-store, x509-validation }:
      mkDerivation {
          pname = "tls";
          version = "1.3.11";
          sha256 = "3f008eb942874f8114f9a332f9669c44d72825ba39ce0fad89f0f8dfa6fb2703";
          libraryHaskellDepends = [
            asn1-encoding
            asn1-types
            async
            base
            bytestring
            cereal
            cryptonite
            data-default-class
            memory
            mtl
            network
            transformers
            x509
            x509-store
            x509-validation
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-tls";
          description = "TLS/SSL protocol native implementation (Server and Client)";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      tls-session-manager = callPackage ({ auto-update, base, clock, mkDerivation, psqueues, stdenv, tls }:
      mkDerivation {
          pname = "tls-session-manager";
          version = "0.0.0.1";
          sha256 = "68261cbedd5fd7013e33f30810f274cbfb4518da02408d31b247867320371b2f";
          revision = "1";
          editedCabalFile = "0hnhxfqmvkkhf37rr2ir52xyd59070jjm6s6al0alsanid2m4p01";
          libraryHaskellDepends = [
            auto-update
            base
            clock
            psqueues
            tls
          ];
          doHaddock = false;
          doCheck = false;
          description = "In-memory TLS session manager";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      transformers = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "transformers";
          version = "0.5.5.0";
          sha256 = "b11eb8827cfd48a801516adec27e2de4091f424386e4c99846c587fc108b19a5";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Concrete functor and monad transformers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      transformers-base = callPackage ({ base, mkDerivation, stdenv, stm, transformers, transformers-compat }:
      mkDerivation {
          pname = "transformers-base";
          version = "0.4.4";
          sha256 = "6aa3494fc70659342fbbb163035d5827ecfd8079e3c929e2372adf771fd52387";
          revision = "1";
          editedCabalFile = "196pr3a4lhgklyw6nq6rv1j9djwzmvx7xrpp58carxnb55gk06pv";
          libraryHaskellDepends = [
            base
            stm
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/mvv/transformers-base";
          description = "Lift computations from the bottom of a transformer stack";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      transformers-compat = callPackage ({ base, ghc-prim, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "transformers-compat";
          version = "0.5.1.4";
          sha256 = "d881ef4ec164b631591b222efe7ff555af6d5397c9d86475b309ba9402a8ca9f";
          libraryHaskellDepends = [
            base
            ghc-prim
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/transformers-compat/";
          description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      transformers-lift = callPackage ({ base, mkDerivation, stdenv, transformers, writer-cps-transformers }:
      mkDerivation {
          pname = "transformers-lift";
          version = "0.2.0.1";
          sha256 = "0bd8bf23fb29874daf9ff990bf25035e21208cfa292f9f18e8cfdb0b4b1ee09d";
          libraryHaskellDepends = [
            base
            transformers
            writer-cps-transformers
          ];
          doHaddock = false;
          doCheck = false;
          description = "Ad-hoc type classes for lifting";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      tuple = callPackage ({ OneTuple, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "tuple";
          version = "0.3.0.2";
          sha256 = "2fcb068ffafbe64170e02094a363f83d1725f44f8af963d9dad943a592e89624";
          libraryHaskellDepends = [
            base
            OneTuple
          ];
          doHaddock = false;
          doCheck = false;
          description = "Various functions on tuples";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      type-operators = callPackage ({ base, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "type-operators";
          version = "0.1.0.4";
          sha256 = "dbbcedf368c23c46abac04f157cb4f2c812099a4f75d606b24f1ac1116d40b74";
          libraryHaskellDepends = [
            base
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/Shou/type-operators#readme";
          description = "Various type-level operators";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      unbounded-delays = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "unbounded-delays";
          version = "0.1.1.0";
          sha256 = "8aa7f7d10a8d0073518804db76c3ef4c313359994ef175122341b0bce07329c7";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/unbounded-delays";
          description = "Unbounded thread delays and timeouts";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      unexceptionalio = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "unexceptionalio";
          version = "0.3.0";
          sha256 = "927e2be6bb9ced73c1c17d79c981cadef4039d9ee45d2d3d6b4c133ff93ff0b8";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/singpolyma/unexceptionalio";
          description = "IO without any non-error, synchronous exceptions";
          license = "unknown";
        }) {};
      universum = callPackage ({ base, bytestring, containers, deepseq, exceptions, ghc-prim, hashable, microlens, microlens-mtl, mkDerivation, mtl, safe, safe-exceptions, stdenv, stm, text, text-format, transformers, type-operators, unordered-containers, utf8-string, vector }:
      mkDerivation {
          pname = "universum";
          version = "0.6.1";
          sha256 = "538bff64be0fba8902cd7c5b0fc40d50848567886078227a5ee388ce9a9f04a3";
          libraryHaskellDepends = [
            base
            bytestring
            containers
            deepseq
            exceptions
            ghc-prim
            hashable
            microlens
            microlens-mtl
            mtl
            safe
            safe-exceptions
            stm
            text
            text-format
            transformers
            type-operators
            unordered-containers
            utf8-string
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/serokell/universum";
          description = "Custom prelude used in Serokell";
          license = stdenv.lib.licenses.mit;
        }) {};
      unix = callPackage ({ base, bytestring, mkDerivation, stdenv, time }:
      mkDerivation {
          pname = "unix";
          version = "2.7.2.1";
          sha256 = "fc05365594367779122465eee132162267c319c3679ff801f050ed30d18d099c";
          revision = "1";
          editedCabalFile = "1m6gvvsb7ds25qws07wn6v3icksmh9g09qbrz726z8rnvvlbdc9x";
          libraryHaskellDepends = [
            base
            bytestring
            time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/unix";
          description = "POSIX functionality";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      unix-compat = callPackage ({ base, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "unix-compat";
          version = "0.4.3.1";
          sha256 = "72801d5a654a6e108c153f412ebd54c37fb445643770e0b97701a59e109f7e27";
          revision = "2";
          editedCabalFile = "0b5jicn8nm53yxxzwlvfcv4xp5rrqp98x5wwqh234wn9x44z54d2";
          libraryHaskellDepends = [
            base
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/jystic/unix-compat";
          description = "Portable POSIX-compatibility layer";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      unix-time = callPackage ({ base, binary, bytestring, mkDerivation, old-time, stdenv }:
      mkDerivation {
          pname = "unix-time";
          version = "0.3.7";
          sha256 = "1131301131dd3e73353a346daa04578ec067073e7674d447051ac1a87262b4e1";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            old-time
          ];
          doHaddock = false;
          doCheck = false;
          description = "Unix time parser/formatter and utilities";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      unordered-containers = callPackage ({ base, deepseq, hashable, mkDerivation, stdenv }:
      mkDerivation {
          pname = "unordered-containers";
          version = "0.2.8.0";
          sha256 = "a4a188359ff28640359131061953f7dbb8258da8ecf0542db0d23f08bfa6eea8";
          libraryHaskellDepends = [
            base
            deepseq
            hashable
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/tibbe/unordered-containers";
          description = "Efficient hashing-based container types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      uri-bytestring = callPackage ({ attoparsec, base, blaze-builder, bytestring, containers, mkDerivation, stdenv, template-haskell, th-lift-instances }:
      mkDerivation {
          pname = "uri-bytestring";
          version = "0.2.3.3";
          sha256 = "3d838bf247e95a66885d2d603c1594ef01d4dade728aa50b6c2224a65d8d0b14";
          libraryHaskellDepends = [
            attoparsec
            base
            blaze-builder
            bytestring
            containers
            template-haskell
            th-lift-instances
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/Soostone/uri-bytestring";
          description = "Haskell URI parsing as ByteStrings";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      utf8-string = callPackage ({ base, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "utf8-string";
          version = "1.0.1.1";
          sha256 = "fb0b9e3acbe0605bcd1c63e51f290a7bbbe6628dfa3294ff453e4235fbaef140";
          revision = "3";
          editedCabalFile = "02vhj5gykkqa2dyn7s6gn8is1b5fdn9xcqqvlls268g7cpv6rk38";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/glguy/utf8-string/";
          description = "Support for reading and writing UTF8 Strings";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      uuid = callPackage ({ base, binary, bytestring, cryptohash-md5, cryptohash-sha1, entropy, mkDerivation, network-info, random, stdenv, text, time, uuid-types }:
      mkDerivation {
          pname = "uuid";
          version = "1.3.13";
          sha256 = "dfac808a7026217d018b408eab18facc6a85c6183be308d4ac7877e80599b027";
          revision = "1";
          editedCabalFile = "0yp01hzsw07d9ismqqkkzwqllfnyyhzhjmwhbhgmkb6v7y7iqrbm";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            cryptohash-md5
            cryptohash-sha1
            entropy
            network-info
            random
            text
            time
            uuid-types
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hvr/uuid";
          description = "For creating, comparing, parsing and printing Universally Unique Identifiers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      uuid-types = callPackage ({ base, binary, bytestring, deepseq, hashable, mkDerivation, random, stdenv, text }:
      mkDerivation {
          pname = "uuid-types";
          version = "1.0.3";
          sha256 = "9276517ab24a9b06f39d6e3c33c6c2b4ace1fc2126dbc1cd9806866a6551b3fd";
          revision = "1";
          editedCabalFile = "0iwwj07gp28g357hv76k4h8pvlzamvchnw003cv3qk778pcpx201";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            deepseq
            hashable
            random
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/aslatter/uuid";
          description = "Type definitions for Universally Unique Identifiers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      vault = callPackage ({ base, containers, hashable, mkDerivation, stdenv, unordered-containers }:
      mkDerivation {
          pname = "vault";
          version = "0.3.0.7";
          sha256 = "9e9189da0821d68fc8f85aab958bbec141635858a7aeb8178e1eec5872a366f0";
          libraryHaskellDepends = [
            base
            containers
            hashable
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/HeinrichApfelmus/vault";
          description = "a persistent store for values of arbitrary types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      vector = callPackage ({ base, deepseq, ghc-prim, mkDerivation, primitive, semigroups, stdenv }:
      mkDerivation {
          pname = "vector";
          version = "0.12.0.1";
          sha256 = "b100ee79b9da2651276278cd3e0f08a3c152505cc52982beda507515af173d7b";
          revision = "1";
          editedCabalFile = "1xjv8876kx9vh86w718vdaaai40pwnsiw8368c5h88ch8iqq10qb";
          libraryHaskellDepends = [
            base
            deepseq
            ghc-prim
            primitive
            semigroups
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/vector";
          description = "Efficient Arrays";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      vector-algorithms = callPackage ({ base, bytestring, mkDerivation, mtl, mwc-random, primitive, stdenv, vector }:
      mkDerivation {
          pname = "vector-algorithms";
          version = "0.7.0.1";
          sha256 = "ed460a41ca068f568bc2027579ab14185fbb72c7ac469b5179ae5f8a52719070";
          revision = "1";
          editedCabalFile = "1996aj239vasr4hd5c0pi9i0bd08r6clzr76nqvf3hc5kjs7vml2";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            bytestring
            primitive
            vector
          ];
          executableHaskellDepends = [
            base
            mtl
            mwc-random
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://code.haskell.org/~dolio/";
          description = "Efficient algorithms for vector arrays";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      vector-binary-instances = callPackage ({ base, binary, mkDerivation, stdenv, vector }:
      mkDerivation {
          pname = "vector-binary-instances";
          version = "0.2.3.5";
          sha256 = "e11255baeca51fb01df28b120ee308802d4e45929e520c8464e3f74513682a5a";
          revision = "1";
          editedCabalFile = "0yk61mifvcc31vancsfsd0vskqh5k3a3znx1rbz8wzcs4ijjzh48";
          libraryHaskellDepends = [
            base
            binary
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/vector-binary-instances";
          description = "Instances of Data.Binary and Data.Serialize for vector";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      vector-th-unbox = callPackage ({ base, mkDerivation, stdenv, template-haskell, vector }:
      mkDerivation {
          pname = "vector-th-unbox";
          version = "0.2.1.6";
          sha256 = "be87d4a6f1005ee2d0de6adf521e05c9e83c441568a8a8b60c79efe24ae90235";
          libraryHaskellDepends = [
            base
            template-haskell
            vector
          ];
          doHaddock = false;
          doCheck = false;
          description = "Deriver for Data.Vector.Unboxed using Template Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      void = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "void";
          version = "0.7.2";
          sha256 = "d3fffe66a03e4b53db1e459edf75ad8402385a817cae415d857ec0b03ce0cf2b";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/void";
          description = "A Haskell 98 logically uninhabited data type";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      wai = callPackage ({ base, blaze-builder, bytestring, bytestring-builder, http-types, mkDerivation, network, stdenv, text, transformers, vault }:
      mkDerivation {
          pname = "wai";
          version = "3.2.1.1";
          sha256 = "5d80a68f5d8806682d8267b7dacc383d094e3ef7ecd705f20e42c91cad564e21";
          libraryHaskellDepends = [
            base
            blaze-builder
            bytestring
            bytestring-builder
            http-types
            network
            text
            transformers
            vault
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/yesodweb/wai";
          description = "Web Application Interface";
          license = stdenv.lib.licenses.mit;
        }) {};
      wai-app-static = callPackage ({ base, blaze-builder, blaze-html, blaze-markup, bytestring, containers, cryptonite, directory, file-embed, filepath, http-date, http-types, memory, mime-types, mkDerivation, old-locale, optparse-applicative, stdenv, template-haskell, text, time, transformers, unix-compat, unordered-containers, wai, wai-extra, warp, zlib }:
      mkDerivation {
          pname = "wai-app-static";
          version = "3.1.6.1";
          sha256 = "b318acf31e2e809411f119744a016ba0a78f52554ac7321a3a1410a218886668";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            blaze-builder
            blaze-html
            blaze-markup
            bytestring
            containers
            cryptonite
            directory
            file-embed
            filepath
            http-date
            http-types
            memory
            mime-types
            old-locale
            optparse-applicative
            template-haskell
            text
            time
            transformers
            unix-compat
            unordered-containers
            wai
            wai-extra
            warp
            zlib
          ];
          executableHaskellDepends = [
            base
            bytestring
            containers
            directory
            mime-types
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.yesodweb.com/book/web-application-interface";
          description = "WAI application for static serving";
          license = stdenv.lib.licenses.mit;
        }) {};
      wai-cors = callPackage ({ attoparsec, base, base-unicode-symbols, bytestring, case-insensitive, http-types, mkDerivation, mtl, stdenv, transformers, wai }:
      mkDerivation {
          pname = "wai-cors";
          version = "0.2.5";
          sha256 = "25089b8a043e0eb042ef63070ddd7ecc4d961c74526c9c00b292eda4b92d766e";
          enableSeparateDataOutput = true;
          libraryHaskellDepends = [
            attoparsec
            base
            base-unicode-symbols
            bytestring
            case-insensitive
            http-types
            mtl
            transformers
            wai
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/larskuhtz/wai-cors";
          description = "CORS for WAI";
          license = stdenv.lib.licenses.mit;
        }) {};
      wai-extra = callPackage ({ aeson, ansi-terminal, base, base64-bytestring, blaze-builder, bytestring, case-insensitive, containers, cookie, data-default-class, deepseq, directory, fast-logger, http-types, iproute, lifted-base, mkDerivation, network, old-locale, resourcet, stdenv, streaming-commons, stringsearch, text, time, transformers, unix, unix-compat, vault, void, wai, wai-logger, word8, zlib }:
      mkDerivation {
          pname = "wai-extra";
          version = "3.0.20.0";
          sha256 = "ad63ca529e812f5edec84e197a58433095a1376a127f8e9416235028bf021971";
          libraryHaskellDepends = [
            aeson
            ansi-terminal
            base
            base64-bytestring
            blaze-builder
            bytestring
            case-insensitive
            containers
            cookie
            data-default-class
            deepseq
            directory
            fast-logger
            http-types
            iproute
            lifted-base
            network
            old-locale
            resourcet
            streaming-commons
            stringsearch
            text
            time
            transformers
            unix
            unix-compat
            vault
            void
            wai
            wai-logger
            word8
            zlib
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/yesodweb/wai";
          description = "Provides some basic WAI handlers and middleware";
          license = stdenv.lib.licenses.mit;
        }) {};
      wai-logger = callPackage ({ base, blaze-builder, byteorder, bytestring, case-insensitive, fast-logger, http-types, mkDerivation, network, stdenv, unix, unix-time, wai }:
      mkDerivation {
          pname = "wai-logger";
          version = "2.3.0";
          sha256 = "90cd993c657e72a0480a988220b288aeb2561efa53d2c8f819197b6de3060bf0";
          libraryHaskellDepends = [
            base
            blaze-builder
            byteorder
            bytestring
            case-insensitive
            fast-logger
            http-types
            network
            unix
            unix-time
            wai
          ];
          doHaddock = false;
          doCheck = false;
          description = "A logging system for WAI";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      wai-websockets = callPackage ({ base, blaze-builder, bytestring, case-insensitive, file-embed, http-types, mkDerivation, network, stdenv, text, transformers, wai, wai-app-static, warp, websockets }:
      mkDerivation {
          pname = "wai-websockets";
          version = "3.0.1.1";
          sha256 = "6abeafea574d9e8f4d41de091afec4594489877aa8717f97e91af5543fd38a31";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            blaze-builder
            bytestring
            case-insensitive
            http-types
            network
            transformers
            wai
            websockets
          ];
          executableHaskellDepends = [
            base
            blaze-builder
            bytestring
            case-insensitive
            file-embed
            http-types
            network
            text
            transformers
            wai
            wai-app-static
            warp
            websockets
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/yesodweb/wai";
          description = "Provide a bridge between WAI and the websockets package";
          license = stdenv.lib.licenses.mit;
        }) {};
      warp = callPackage ({ array, async, auto-update, base, blaze-builder, bytestring, bytestring-builder, case-insensitive, containers, ghc-prim, hashable, http-date, http-types, http2, iproute, mkDerivation, network, simple-sendfile, stdenv, stm, streaming-commons, text, unix, unix-compat, vault, wai, word8 }:
      mkDerivation {
          pname = "warp";
          version = "3.2.13";
          sha256 = "92395bf42d012e5c4deaea7f9e1fc2271a63c5380b4c5bc1cf16b7c53aa2c424";
          libraryHaskellDepends = [
            array
            async
            auto-update
            base
            blaze-builder
            bytestring
            bytestring-builder
            case-insensitive
            containers
            ghc-prim
            hashable
            http-date
            http-types
            http2
            iproute
            network
            simple-sendfile
            stm
            streaming-commons
            text
            unix
            unix-compat
            vault
            wai
            word8
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/yesodweb/wai";
          description = "A fast, light-weight web server for WAI applications";
          license = stdenv.lib.licenses.mit;
        }) {};
      warp-tls = callPackage ({ base, bytestring, cryptonite, data-default-class, mkDerivation, network, stdenv, streaming-commons, tls, tls-session-manager, wai, warp }:
      mkDerivation {
          pname = "warp-tls";
          version = "3.2.4";
          sha256 = "05d1aad58fa1a16a652369d7247d4c68b86af0b8febaea9ab7969c121f956e17";
          libraryHaskellDepends = [
            base
            bytestring
            cryptonite
            data-default-class
            network
            streaming-commons
            tls
            tls-session-manager
            wai
            warp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/yesodweb/wai";
          description = "HTTP over TLS support for Warp via the TLS package";
          license = stdenv.lib.licenses.mit;
        }) {};
      websockets = callPackage ({ SHA, attoparsec, base, base64-bytestring, binary, blaze-builder, bytestring, case-insensitive, containers, entropy, mkDerivation, network, random, stdenv, text }:
      mkDerivation {
          pname = "websockets";
          version = "0.10.0.0";
          sha256 = "3ee56fa6683912928a7d336d591c43e4948886037b5aa72cbab2f33fb43fa2eb";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            attoparsec
            base
            base64-bytestring
            binary
            blaze-builder
            bytestring
            case-insensitive
            containers
            entropy
            network
            random
            SHA
            text
          ];
          executableHaskellDepends = [
            attoparsec
            base
            base64-bytestring
            binary
            blaze-builder
            bytestring
            case-insensitive
            containers
            entropy
            network
            random
            SHA
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://jaspervdj.be/websockets";
          description = "A sensible and clean way to write WebSocket-capable servers in Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      wl-pprint-text = callPackage ({ base, base-compat, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "wl-pprint-text";
          version = "1.1.1.0";
          sha256 = "2960c8201c05d912a1df748a3ceeadc7525905ff1c371d7b4972f4011eca0acd";
          libraryHaskellDepends = [
            base
            base-compat
            text
          ];
          doHaddock = false;
          doCheck = false;
          description = "A Wadler/Leijen Pretty Printer for Text values";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      word8 = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "word8";
          version = "0.1.3";
          sha256 = "2630934c75728bfbf390c1f0206b225507b354f68d4047b06c018a36823b5d8a";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Word8 library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      writer-cps-mtl = callPackage ({ base, mkDerivation, mtl, stdenv, transformers, writer-cps-transformers }:
      mkDerivation {
          pname = "writer-cps-mtl";
          version = "0.1.1.4";
          sha256 = "62a3b3b76a5dc0dc6e8b9837afc8c5fc83fb334a034f89fab6a4a544fe204870";
          libraryHaskellDepends = [
            base
            mtl
            transformers
            writer-cps-transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/minad/writer-cps-mtl#readme";
          description = "MonadWriter orphan instances for writer-cps-transformers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      writer-cps-transformers = callPackage ({ base, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "writer-cps-transformers";
          version = "0.1.1.3";
          sha256 = "8aa22832fdb413c706a6862b83ad4a4ef8dd61ae8658aca6e5076cf2a5cd4aae";
          libraryHaskellDepends = [
            base
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/minad/writer-cps-transformers#readme";
          description = "WriteT and RWST monad transformers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      x509 = callPackage ({ asn1-encoding, asn1-parse, asn1-types, base, bytestring, containers, cryptonite, hourglass, memory, mkDerivation, mtl, pem, stdenv }:
      mkDerivation {
          pname = "x509";
          version = "1.7.2";
          sha256 = "dc0315a9e2bbfb2b3b6746b83cde901c0cc6aca5a3983f129c6f1cbe0ee0ce7b";
          revision = "1";
          editedCabalFile = "07mphpmj4zk5mzhp5x50a7q6w134kgymf557dcgbp643cbkcmc66";
          libraryHaskellDepends = [
            asn1-encoding
            asn1-parse
            asn1-types
            base
            bytestring
            containers
            cryptonite
            hourglass
            memory
            mtl
            pem
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-certificate";
          description = "X509 reader and writer";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      x509-store = callPackage ({ asn1-encoding, asn1-types, base, bytestring, containers, cryptonite, directory, filepath, mkDerivation, mtl, pem, stdenv, x509 }:
      mkDerivation {
          pname = "x509-store";
          version = "1.6.5";
          sha256 = "1aaab11da87f8c27b7475c4b0789823864e5f215fd5bf7c8a455feba807fe9d1";
          libraryHaskellDepends = [
            asn1-encoding
            asn1-types
            base
            bytestring
            containers
            cryptonite
            directory
            filepath
            mtl
            pem
            x509
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-certificate";
          description = "X.509 collection accessing and storing methods";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      x509-system = callPackage ({ base, bytestring, containers, directory, filepath, mkDerivation, mtl, pem, process, stdenv, x509, x509-store }:
      mkDerivation {
          pname = "x509-system";
          version = "1.6.6";
          sha256 = "40dcdaae3ec67f38c08d96d4365b901eb8ac0c590bd7972eb429d37d58aa4419";
          libraryHaskellDepends = [
            base
            bytestring
            containers
            directory
            filepath
            mtl
            pem
            process
            x509
            x509-store
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-certificate";
          description = "Handle per-operating-system X.509 accessors and storage";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      x509-validation = callPackage ({ asn1-encoding, asn1-types, base, byteable, bytestring, containers, cryptonite, data-default-class, hourglass, memory, mkDerivation, mtl, pem, stdenv, x509, x509-store }:
      mkDerivation {
          pname = "x509-validation";
          version = "1.6.9";
          sha256 = "8470cead7cf0c8cd93137f1edeb1603805d54f50647b15331d9d952fbb2cb500";
          revision = "1";
          editedCabalFile = "02n9s0wizi4wivs6is4cyapqjjnbrx3zdk34q0cnlfsvbbvyhjax";
          libraryHaskellDepends = [
            asn1-encoding
            asn1-types
            base
            byteable
            bytestring
            containers
            cryptonite
            data-default-class
            hourglass
            memory
            mtl
            pem
            x509
            x509-store
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-certificate";
          description = "X.509 Certificate and CRL validation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      xml = callPackage ({ base, bytestring, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "xml";
          version = "1.3.14";
          sha256 = "32d1a1a9f21a59176d84697f96ae3a13a0198420e3e4f1c48abbab7d2425013d";
          libraryHaskellDepends = [
            base
            bytestring
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://code.galois.com";
          description = "A simple XML library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      yaml = callPackage ({ aeson, attoparsec, base, bytestring, conduit, containers, directory, filepath, mkDerivation, resourcet, scientific, semigroups, stdenv, template-haskell, text, transformers, unordered-containers, vector }:
      mkDerivation {
          pname = "yaml";
          version = "0.8.23.3";
          sha256 = "cc855c7ed50cbc4c706400f98854087ce7b8eadbd98646001a15c13c11ed7543";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            attoparsec
            base
            bytestring
            conduit
            containers
            directory
            filepath
            resourcet
            scientific
            semigroups
            template-haskell
            text
            transformers
            unordered-containers
            vector
          ];
          executableHaskellDepends = [
            aeson
            base
            bytestring
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/snoyberg/yaml/";
          description = "Support for parsing and rendering YAML documents";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      zlib = callPackage ({ base, bytestring, mkDerivation, stdenv, zlib }:
      mkDerivation {
          pname = "zlib";
          version = "0.6.1.2";
          sha256 = "e4eb4e636caf07a16a9730ce469a00b65d5748f259f43edd904dd457b198a2bb";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          librarySystemDepends = [ zlib ];
          doHaddock = false;
          doCheck = false;
          description = "Compression and decompression in the gzip and zlib formats";
          license = stdenv.lib.licenses.bsd3;
        }) { zlib = pkgs.zlib; };
    };
in
compiler.override {
  initialPackages = stackPackages;
  configurationCommon = { ... }: self: super: {};
}

