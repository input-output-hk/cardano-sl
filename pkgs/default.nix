# Generated using stack2nix 0.1.3.0.
#
# Only works with sufficiently recent nixpkgs, e.g. "NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/21a8239452adae3a4717772f4e490575586b2755.tar.gz".

{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc802
, ghc ? pkgs.haskell.compiler.ghc802
}:

with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let
  stackPackages = { callPackage, pkgs, stdenv }:
self: {
      Cabal = callPackage ({ array, base, binary, bytestring, containers, deepseq, directory, filepath, mkDerivation, pretty, process, stdenv, time, unix }:
      mkDerivation {
          pname = "Cabal";
          version = "1.24.2.0";
          sha256 = "0h33v1716wkqh9wvq2wynvhwzkjjhg4aav0a1i3cmyq36n7fpl5p";
          revision = "1";
          editedCabalFile = "0jw809psa2ms9sy1mnirmbj9h7rs76wbmf24zgjqvhp4wq919z3m";
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
          sha256 = "13g27db8ln2n1hr2wvpa855x0x1izk3c10lb85an7972b5lw2hl4";
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
          sha256 = "0hczp9dj9qs3g72hcgikym1bq3ki90graxfx068h5hds0kn1s66a";
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
          version = "0.7.14";
          sha256 = "0aw43izg8vlvjl40ms6k92w7gxg7n3l6smdvzla47fp82s4vhdr8";
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
          sha256 = "186ykl7vxlfgkd2k8k1rq7yzcryzjpqwmn4ci1nn9h6irqbivib5";
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
          version = "3.2.8.1";
          sha256 = "013il9pnglb2mbiz5bbk1wg4mw2yvyi07fjv2zvmmml7p18fs9dk";
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
          sha256 = "11qdfghizww810vdj9ac1f5qr5kdmrk40l6w6qh311bjh290ygwy";
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
          sha256 = "1x52b68zh3k9lnps5s87kzan7dzvqp6mrwgayjq15w9dv6v78vsb";
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
          sha256 = "119np67qvx8hyp9vkg4gr2wv3lj3j6ay2vl4hxspkg43ymb1cp0m";
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
      SHA = callPackage ({ array, base, binary, bytestring, mkDerivation, stdenv }:
      mkDerivation {
          pname = "SHA";
          version = "1.6.4.2";
          sha256 = "134ajm87fm4lpsw86m9q8apv20dw4bpk46raa389zr6bcdpifw64";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            array
            base
            binary
            bytestring
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
          sha256 = "1w6hh8anpb0psilzbp4k80rbavdmkmb5rn34x9m2s72rz0jfy9zp";
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
          sha256 = "1dzz9l0haswgag9x56q7n57kw18v7nhmzkjyr61nz9y9npn8vmks";
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
          sha256 = "18jwswjxwzc9bjiy4ds6hw2a74ki797jmfcifxd2ga4kh7ri1ah9";
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
          sha256 = "0q6qsniw4wks2pw6wzncb1p1j3k6al5njnvm2v5n494hplwqg2i4";
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
          sha256 = "1yw029rh0gb63bhwwjynbv173mny14is4cyjkrlvzvxwb0fi96jx";
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
          sha256 = "1k1ykisf96i4g2zm47c45md7p42c4vsp9r73392pz1g8mx7s2j5r";
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
      aeson = callPackage ({ attoparsec, base, base-compat, bytestring, containers, deepseq, dlist, ghc-prim, hashable, mkDerivation, scientific, stdenv, tagged, template-haskell, text, time, time-locale-compat, unordered-containers, vector }:
      mkDerivation {
          pname = "aeson";
          version = "1.0.2.1";
          sha256 = "0rlhr225vb6apxw1m0jpnjpbcwb2ij30n6r41qyhd5lr1ax6z9p0";
          revision = "1";
          editedCabalFile = "1wfplxb4bppx2bqxbwprl09w9h9bfwn4ak97g8nrdrm30xfqv16g";
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
          version = "0.8.2";
          sha256 = "1c5r1w1hcv297pmj9yjpz9al22k3mh61gimi37wddga02212kd3c";
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
      ansi-terminal = callPackage ({ base, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "ansi-terminal";
          version = "0.6.2.3";
          sha256 = "0hpfw0k025y681m9ml1c712skrb1p4vh7z5x1f0ci9ww7ssjrh2d";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            unix
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
          sha256 = "025pyphsjf0dnbrmj5nscbi6gzyigwgp3ifxb3psn7kji6mfr29p";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
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
          sha256 = "09jb9ij78fdkz2qk66rw99q19qnm504dpv0yq0pjsl6xwjmndsjq";
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
          sha256 = "08r2rq4blvc737mrg3xhlwiw13jmsz5dlf2fd0ghb9cdaxc6kjc9";
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
          sha256 = "0adgbamyq0mj1l1hdq4zyyllay714bac1wl0rih3fv1z6vykp1hy";
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
          sha256 = "025prsihk5g6rdv9xlfmj0zpa0wa3qjzj5i4ilzvg7f6f3sji8y6";
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
          sha256 = "05vjchyqiy9n275cygffhn0ma7fz7jx52j0dcdm9qm8h9bziymqc";
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
          sha256 = "1qj4fp1ynwg0l453gmm27vgkzb5k5m2hzdlg5rdqi9kf8rqy90yd";
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
          sha256 = "0r1zrrkbqv8w4pb459fj5izd1h85p9nrsp3gyzj7qiayjpa79p2j";
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
          sha256 = "12l55b76bhya9q89mfmqmy6sl5v39b6gzrw5rf3f70vkb23nsv5a";
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
          sha256 = "09dlh2alsx2mw5kvj931yhbj0aw7jmly2cm9xbscm2sf098w35jy";
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
          version = "1.0.0";
          sha256 = "1wx0mdw0dqa9brgznfvnpcaf4pka87nbkrqxbyi94fhlpcdsz6s5";
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
          sha256 = "0zpvf4yq52dkl9f30w6x4fv1lqcc175i57prhv56ky06by08anvs";
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
          sha256 = "0452l6zf6fjhy4kxqwv6i6hhg6yfx4wcg450k3axpyj30l7jnq3x";
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
          version = "0.5.4";
          sha256 = "0qv20n4yabg7sc3rs2dd46a53c7idnd88by7n3s36dkbc21m41q4";
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
          version = "1.0.1.1";
          sha256 = "1d9iga5nj66h295j09q0wh246ahagjcqfv9br6x51ya57fd0mkyw";
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
          sha256 = "1afc5pchd3vw33bmjbjygkd0l5zh7glbsx4bfyxfscpc1x1l3y52";
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
          sha256 = "0jf40m3yijqw6wd1rwwvviww46fasphaay9m9rgqyhf5aahnbzjs";
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
          sha256 = "1ls05nzswjr6aw0wwk3q7cpv1hf0lw7vk16a5khm6l21yfcgbny2";
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
          sha256 = "0l1v4ddjdsgi9nqzyzcxxj76rwar3lzx8gmwf2r54bqan3san9db";
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
          sha256 = "13fwvw1102ik96pgi85i34kisz1h237vgw88ywsgifsah9kh4qiq";
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
          sha256 = "08d85qzna6zdkpgqwaw1d87biviv1b76zvk5qs3gg4kxwzfqa4r2";
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
          sha256 = "1m33y6p5xldni8p4fzg8fmsyqvkfmnimdamr1xjnsmgm3dkf9lws";
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
          version = "0.8.1.3";
          sha256 = "0dyn6cj5av4apmc3wav6asfap53gxy4hzdb7rph83yakscbyf5lc";
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
          version = "0.7.1.1";
          sha256 = "00s3qlkbq9gxgy6l5skbhnl5h81mjgzqcrw3yn3wqnyd9scab3b3";
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
          sha256 = "0q5a4wam0sidng0cfsivwkyph9snyilk7rsdx4vb6wz9l6xz397n";
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
          sha256 = "1qizg0kxxjqnd3cbrjhhidk5pbbciz0pb3z5kzikjjxnnnhk8fr4";
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
          sha256 = "06995paxbxk8lldvarqpb3ygcjbg4v8dk4scib1rjzwlhssvn85x";
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
          sha256 = "0kfdw1c13y3kxc1s9nzyavrv1ccipzrmqlwmigj3gnwjcjvddp6q";
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
          sha256 = "16zwb1p83z7vc5wlhvknpy80b5a2jxc5awx67rk52qnp9idmyq9d";
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
          sha256 = "1hnvjac28y44yn78c9vdp1zvrknvlw98ky3g4n5vivr16rvh8x3d";
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
          version = "1";
          sha256 = "0iv1rjn79mh8v0pysa20nx3571sgls081x483n3gh44x09i9sxkw";
          revision = "1";
          editedCabalFile = "0g17mir6wa8vk2sgdhxba2f4g6wscbp7pib8rdpkq2asx48qbsnf";
          libraryHaskellDepends = [
            base
            Cabal
            directory
            filepath
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/cabal-doctests";
          description = "A Setup.hs helper for doctests running";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      call-stack = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "call-stack";
          version = "0.1.0";
          sha256 = "1qmihf5jafmc79sk52l6gpx75f5bnla2lp62kh3p34x3j84mwpzj";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/sol/call-stack#readme";
          description = "Use GHC call-stacks in a backward compatible way";
          license = stdenv.lib.licenses.mit;
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
          version = "0.2.1";
          src = fetchgit {
            url = "https://github.com/input-output-hk/cardano-report-server.git";
            sha256 = "02mf9nw5n0lcq9p6j33lsn0vbab4ai4z3j2099qlzcaqf3kq1987";
            rev = "c2af07ab7d627556ed3f6185b062e4cd1fb5ad26";
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
      cardano-sl = callPackage ({ MonadRandom, QuickCheck, acid-state, aeson, ansi-terminal, ansi-wl-pprint, async, base, base58-bytestring, base64-bytestring, binary, bytestring, cardano-crypto, cardano-report-server, cardano-sl-core, cardano-sl-db, cardano-sl-godtossing, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, cardano-sl-txp, cardano-sl-update, cborg, cereal, conduit, containers, cpphs, cryptonite, cryptonite-openssl, data-default, deepseq, deriving-compat, digest, directory, dlist, dns, ed25519, ekg, ekg-core, ekg-statsd, ether, exceptions, file-embed, filelock, filepath, focus, formatting, generic-arbitrary, gitrev, hashable, hspec, http-client, http-client-tls, http-conduit, http-types, iproute, kademlia, lens, list-t, log-warper, lrucache, memory, mkDerivation, mmorph, monad-control, monad-loops, mono-traversable, mtl, neat-interpolation, network-info, network-transport, network-transport-tcp, node-sketch, optparse-applicative, parsec, plutus-prototype, pvss, quickcheck-instances, random, reflection, regex-tdfa, regex-tdfa-text, resourcet, rocksdb-haskell, safecopy, serokell-util, servant, servant-multipart, servant-server, stdenv, stm, stm-containers, string-qq, tagged, template-haskell, text, text-format, th-lift-instances, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers, vector, wai, wai-extra, warp, warp-tls, yaml }:
      mkDerivation {
          pname = "cardano-sl";
          version = "0.5.1";
          src = ./../.;
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
            ekg
            ekg-core
            ekg-statsd
            ether
            exceptions
            file-embed
            filelock
            filepath
            focus
            formatting
            generic-arbitrary
            gitrev
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
            safecopy
            serokell-util
            servant
            servant-multipart
            servant-server
            stm
            stm-containers
            string-qq
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
            base
            bytestring
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
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL main implementation";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-core = callPackage ({ QuickCheck, aeson, autoexporter, base, base58-bytestring, binary, bytestring, cardano-crypto, cborg, cereal, concurrent-extra, containers, contravariant, cpphs, cryptonite, cryptonite-openssl, data-default, deepseq, deriving-compat, digest, directory, ed25519, ether, exceptions, file-embed, filepath, formatting, generic-arbitrary, hashable, lens, log-warper, lrucache, memory, mkDerivation, mmorph, mtl, node-sketch, parsec, plutus-prototype, pvss, quickcheck-instances, random, reflection, resourcet, safecopy, semigroups, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, th-utilities, time, time-units, transformers, transformers-base, transformers-lift, universum, unordered-containers, vector, yaml }:
      mkDerivation {
          pname = "cardano-sl-core";
          version = "0.5.1";
          src = ./../core;
          libraryHaskellDepends = [
            aeson
            autoexporter
            base
            base58-bytestring
            binary
            bytestring
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
            semigroups
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
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
          version = "0.5.1";
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
      cardano-sl-explorer = callPackage ({ aeson, base, base16-bytestring, binary, bytestring, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-ssc, cardano-sl-update, containers, cpphs, either, engine-io, engine-io-wai, ether, exceptions, formatting, http-types, lens, lifted-base, log-warper, memory, mkDerivation, monad-control, monad-loops, mtl, network-transport-tcp, node-sketch, optparse-simple, purescript-bridge, serokell-util, servant, servant-multipart, servant-server, servant-swagger, servant-swagger-ui, socket-io, stdenv, stm, swagger2, tagged, text, text-format, time, time-units, transformers, universum, unordered-containers, wai, wai-cors, warp }:
      mkDerivation {
          pname = "cardano-sl-explorer";
          version = "0.2.0";
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
            cardano-sl-infra
            cardano-sl-ssc
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
            warp
          ];
          libraryToolDepends = [ cpphs ];
          executableHaskellDepends = [
            aeson
            base
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-infra
            cardano-sl-ssc
            cardano-sl-update
            containers
            ether
            formatting
            lens
            log-warper
            mtl
            network-transport-tcp
            node-sketch
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
          doHaddock = false;
          doCheck = true;
          description = "Cardano explorer";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-godtossing = callPackage ({ QuickCheck, aeson, base, bytestring, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, containers, cpphs, cryptonite, data-default, ether, file-embed, formatting, generic-arbitrary, hashable, lens, log-warper, mkDerivation, mmorph, mono-traversable, mtl, node-sketch, rocksdb-haskell, serokell-util, stdenv, stm, tagged, text, text-format, time-units, transformers, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-godtossing";
          version = "0.5.1";
          src = ./../godtossing;
          libraryHaskellDepends = [
            aeson
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
            ether
            file-embed
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
            rocksdb-haskell
            serokell-util
            stm
            tagged
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
      cardano-sl-infra = callPackage ({ QuickCheck, aeson, base, base64-bytestring, binary, bytestring, cardano-report-server, cardano-sl-core, cardano-sl-db, containers, cpphs, data-default, directory, dns, either, ether, exceptions, filepath, formatting, generic-arbitrary, hashable, http-client, http-client-tls, iproute, kademlia, lens, list-t, log-warper, mkDerivation, mmorph, monad-control, mtl, network-info, network-transport, network-transport-tcp, node-sketch, optparse-applicative, parsec, reflection, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers, yaml }:
      mkDerivation {
          pname = "cardano-sl-infra";
          version = "0.5.1";
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
      cardano-sl-lrc = callPackage ({ QuickCheck, base, bytestring, cardano-sl-core, cardano-sl-db, conduit, cpphs, ether, formatting, generic-arbitrary, lens, log-warper, mkDerivation, node-sketch, reflection, stdenv, text-format, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-lrc";
          version = "0.5.1";
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
      cardano-sl-lwallet = callPackage ({ QuickCheck, acid-state, ansi-wl-pprint, base, base58-bytestring, binary, bytestring, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-update, containers, cpphs, data-default, dlist, either, ether, formatting, lens, log-warper, mkDerivation, mmorph, monad-control, monad-loops, mtl, neat-interpolation, network-transport-tcp, node-sketch, optparse-applicative, parsec, random, resourcet, safecopy, serokell-util, stdenv, stm, stm-containers, tagged, text, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-lwallet";
          version = "0.5.1";
          src = ./../lwallet;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [
            acid-state
            ansi-wl-pprint
            base
            base58-bytestring
            binary
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-update
            containers
            data-default
            dlist
            either
            ether
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
          description = "Cardano SL - Light wallet";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-ssc = callPackage ({ QuickCheck, base, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cpphs, cryptonite, ether, exceptions, formatting, lens, log-warper, memory, mkDerivation, mmorph, mtl, node-sketch, serokell-util, stdenv, stm, tagged, text-format, universum }:
      mkDerivation {
          pname = "cardano-sl-ssc";
          version = "0.5.1";
          src = ./../ssc;
          libraryHaskellDepends = [
            base
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            cryptonite
            ether
            exceptions
            formatting
            lens
            log-warper
            memory
            mmorph
            mtl
            node-sketch
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
      cardano-sl-tools = callPackage ({ Chart, Chart-diagrams, Glob, MonadRandom, QuickCheck, aeson, ansi-wl-pprint, array, async, attoparsec, base, bytestring, cardano-report-server, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, cardano-sl-txp, containers, cpphs, cryptonite, data-default, directory, ed25519, ether, fgl, filepath, foldl, formatting, graphviz, kademlia, lens, log-warper, mkDerivation, mtl, neat-interpolation, node-sketch, optparse-applicative, parsec, pipes, pipes-bytestring, pipes-interleave, pipes-safe, process, random, random-shuffle, serokell-util, stdenv, stm, system-filepath, tar, text, text-format, time, time-units, universum, unix-compat, unordered-containers, vector }:
      mkDerivation {
          pname = "cardano-sl-tools";
          version = "0.5.1";
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
            bytestring
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
            serokell-util
            stm
            system-filepath
            tar
            text
            text-format
            time
            time-units
            universum
            unix-compat
            unordered-containers
            vector
          ];
          executableToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - Tools";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-txp = callPackage ({ QuickCheck, aeson, base, bytestring, cardano-sl-core, cardano-sl-db, cardano-sl-infra, conduit, containers, cpphs, data-default, ekg-core, ether, formatting, generic-arbitrary, hashable, lens, lifted-base, log-warper, memory, mkDerivation, monad-control, mtl, neat-interpolation, node-sketch, plutus-prototype, resourcet, rocksdb-haskell, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, time-units, transformers, universum, unordered-containers, vector }:
      mkDerivation {
          pname = "cardano-sl-txp";
          version = "0.5.1";
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
      cardano-sl-update = callPackage ({ QuickCheck, aeson, base, binary, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, concurrent-extra, conduit, containers, cpphs, data-default, ether, exceptions, formatting, generic-arbitrary, hashable, lens, log-warper, mkDerivation, mtl, node-sketch, parsec, resourcet, rocksdb-haskell, safecopy, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, th-lift-instances, time-units, transformers, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-update";
          version = "0.5.1";
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
      cardano-sl-wallet = callPackage ({ aeson, ansi-wl-pprint, base, base58-bytestring, binary, bytestring, cardano-report-server, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-ssc, cardano-sl-txp, cardano-sl-update, containers, cpphs, data-default, directory, dlist, ether, exceptions, filepath, formatting, lens, log-warper, mkDerivation, mtl, network-transport, network-transport-tcp, node-sketch, optparse-applicative, parsec, purescript-bridge, random, serokell-util, servant, servant-multipart, servant-server, servant-swagger, servant-swagger-ui, stdenv, stm, stm-containers, string-qq, swagger2, text, text-format, time, time-units, transformers, universum, unix, unordered-containers, wai, wai-websockets, websockets }:
      mkDerivation {
          pname = "cardano-sl-wallet";
          version = "0.5.1";
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
            serokell-util
            servant
            servant-multipart
            servant-server
            stm
            stm-containers
            string-qq
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
            aeson
            ansi-wl-pprint
            base
            base58-bytestring
            binary
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-db
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
          version = "1.2.0.9";
          sha256 = "1f6jjgxnc8579pzf4d96xlg2gk20mrglpdrq85fwsizz113qrpm7";
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
          sha256 = "120ljrwm15zl49nlsn9wb702sb97d2p300mzbpx8wxr2zdzlffpj";
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
          sha256 = "1rzyr8r9pjlgas5pc8n776r22i0ficanq05ypqrs477jxxd6rjns";
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
          sha256 = "0czrb4l1n73cfxxlzbcqfa34qa3gw0m5w5mlz0rawylyqfk8a1pz";
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
          sha256 = "07v91s20halsqjmziqb1sqjp2sjpckl9by7y28aaklwqi2bh2rl8";
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
          sha256 = "1nklhglfa83s9rd8x4j40bvnzdvd81pwdq902sv51mnfyk5a8drl";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
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
          sha256 = "1qmn1778xzg07jg9nx4k1spdz2llivpblf6wwrps1qpqjhsac5cd";
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
          version = "5.0.1";
          sha256 = "0ga67ynh1j4ylbn3awjh7iga09fypbh4fsa21mylcf4xgmlzs7sn";
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
          sha256 = "04nw39pbfqa4ldymn706ij83hxa07c73r7hy18y5pwpmj05cq9vg";
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
          version = "1.2.10";
          sha256 = "1paqps8sc5ilx2nj98svvv5y9p26cl02d2a16qk9m16slzg7l5ni";
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
          version = "1.1.15";
          sha256 = "13dvj271bhdaf83px99mlm0pgvc3474cmidh35jj775m1pmjkvvv";
          revision = "1";
          editedCabalFile = "0kqnggsn4fqvjh5gadaf8h5sw4hprppx63ihpmz32rymhc48sjcl";
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
          sha256 = "1swkb9w5vx9ph7x55y51dc0srj2z27nd9ibgn8c0qcl6hx7g9cbh";
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
      containers = callPackage ({ array, base, deepseq, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "containers";
          version = "0.5.7.1";
          sha256 = "0y8g81p2lx3c2ks2xa798iv0xf6zvks9lc3l6k1jdsp20wrnr1bk";
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
          sha256 = "117fff8kkrvlmr8cb2jpj71z7lf2pdiyks6ilyx89mry6zqnsrp1";
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
          sha256 = "1r2j518lfcswn76qm6p2h1rl98gfsxad7p7z9qaww84fj28k0h86";
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
          version = "1.20.5";
          sha256 = "1cx565wv9r60xw8la5mjfpvsry5wxh9h6ai40jbwd727nwq0r8y5";
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
          sha256 = "1y8q7s2bn4gdknw1wjikdnar2b5pgz3nv3220lxrlgpsf23x82vi";
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
          sha256 = "1aqdxdhxhl9jldh951djpwxx8z7gzaqspxl7iwpl84i5ahrsyy9w";
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
          sha256 = "1680dxgmnjgj083jhsw3rlljwaw0zqi5099m59x6kwqkxhn1qjpf";
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
          sha256 = "19jhhz1ad5jw8zc7ia9bl77g7nw2g0qjk5nmz1zpngpvdg4rgjx8";
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
          sha256 = "1vf2g1gac3rm32g97rl0fll51m88q7ry4m6khnl5j47qsmx24r9l";
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
          sha256 = "04d5n8ybmcxba9qb6h389w9zfq1lvj81b82jh6maqp6pkhkmvydh";
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
          sha256 = "0miyjz8d4jyvqf2vp60lyfbnflx6cj2k8apmm9ly1hq0y0iv80ag";
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
          sha256 = "06h8xka031w752a7cjlzghvr8adqbl95xj9z5zc1b62w02phfpm5";
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
          sha256 = "0narkdqiprhgayjiawrr4390h4rq4pl2pb6mvixbv2phrc8kfs3x";
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
          sha256 = "00h81i5phib741yj517p8mbnc48myvfj8axzsw44k34m48lv1lv0";
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
          sha256 = "0la9x4hvf1rbmxv8h9dk1qln21il3wydz6wbdviryh4h2wls22ny";
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
          sha256 = "0v9m76hjrlrcbyawdp04y1vv0p867h3jhy00xjxgmqq5cm0sn7qc";
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
          sha256 = "1rrak6vym0q1c00cvhdlh29z8vsr6w81lq1xa9b61f5d7m42yl75";
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
      diagrams-lib = callPackage ({ JuicyPixels, active, adjunctions, array, base, cereal, colour, containers, data-default-class, diagrams-core, diagrams-solve, directory, distributive, dual-tree, exceptions, filepath, fingertree, fsnotify, hashable, intervals, lens, linear, mkDerivation, monoid-extras, mtl, optparse-applicative, process, profunctors, semigroups, stdenv, tagged, text, transformers, unordered-containers }:
      mkDerivation {
          pname = "diagrams-lib";
          version = "1.4.0.1";
          sha256 = "1iidlcqb001w2a0a4wnxv1qwybjv23qjx12k8a6hairhr28bah2i";
          revision = "1";
          editedCabalFile = "12jyrrfsxz8mmjf55m7r53vinam88g1rm430ybldldfv7b6sp6dh";
          libraryHaskellDepends = [
            active
            adjunctions
            array
            base
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
          sha256 = "1d4dbwd4qgrlwm0m9spwqklpg3plf0ghrnrah1k6yw900l0z0n7y";
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
          version = "0.1.0.1";
          sha256 = "1piv8mi5182b2ggr28fnr4mda4vbk4mzasvy0nfp3517ii63mrbi";
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
          sha256 = "11vzcsqgkc8jzm5dw82swgqzahck541mz2l9jkkwfdaq09w16sff";
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
          sha256 = "04gy2zp8yzvv7j9bdfvmfzcz3sqyqa6rwslqcn4vyair2vmif5v4";
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
          sha256 = "1wm738bqz8b8mpkviv0y6v6dypxjsm50silfvjwy64c3p9md1c4l";
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
          version = "0.5.2";
          sha256 = "1nbcyysnrkliy7xwx6f39p80kkp0vlvq14wdj6r0m5c1brmbxqmd";
          revision = "2";
          editedCabalFile = "1wp2wqhsshiyxxwil3q0az35vijn8cyd2g6866i32j3p9g01mkr9";
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
          version = "0.8.0.2";
          sha256 = "1ca1hvl5kd4api4gjyhwwavdx8snq6gf1jr6ab0zmjx7p77pwfbp";
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
      dns = callPackage ({ attoparsec, base, binary, bytestring, conduit, conduit-extra, containers, fetchgit, iproute, mkDerivation, mtl, network, random, resourcet, safe, stdenv }:
      mkDerivation {
          pname = "dns";
          version = "2.0.10";
          src = fetchgit {
            url = "https://github.com/kazu-yamamoto/dns.git";
            sha256 = "0hgrrw6l32rgiz5rv0m9dlqvamsw1w32h83ps7nqma7kdmq1j511";
            rev = "c9b06eb77225c8bb2092f1169c093d7a04129e53";
          };
          libraryHaskellDepends = [
            attoparsec
            base
            binary
            bytestring
            conduit
            conduit-extra
            containers
            iproute
            mtl
            network
            random
            resourcet
            safe
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
          sha256 = "0sx2kc1gw72mjvd8vph8bbjw5whfxfv92rsdhjg1c0al75rf3ka4";
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
      dual-tree = callPackage ({ base, mkDerivation, monoid-extras, newtype, semigroups, stdenv }:
      mkDerivation {
          pname = "dual-tree";
          version = "0.2.0.9";
          sha256 = "193ncl8l186gxrfj5pszbmlwjw4gljlkziddql0dyqhr8yxhlzay";
          libraryHaskellDepends = [
            base
            monoid-extras
            newtype
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
          sha256 = "0v75081bx4qzlqy29hh639nzlr7dncwza3qxbzm9njc4jarf31pz";
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
          sha256 = "1lrlwqqnm6ibfcydlv5qvvssw7bm0c6yypy0rayjzv1znq7wp1xh";
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
      ekg = callPackage ({ aeson, base, bytestring, ekg-core, ekg-json, filepath, mkDerivation, network, snap-core, snap-server, stdenv, text, time, transformers, unordered-containers }:
      mkDerivation {
          pname = "ekg";
          version = "0.4.0.13";
          sha256 = "13xlggjcfmp8hr8sz74r0xms36rrfa86znazy2m6304dgscdbca4";
          libraryHaskellDepends = [
            aeson
            base
            bytestring
            ekg-core
            ekg-json
            filepath
            network
            snap-core
            snap-server
            text
            time
            transformers
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/tibbe/ekg";
          description = "Remote monitoring of processes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ekg-core = callPackage ({ base, containers, ghc-prim, mkDerivation, stdenv, text, unordered-containers }:
      mkDerivation {
          pname = "ekg-core";
          version = "0.1.1.1";
          sha256 = "1mir54l783pwy4fbz5bdbckz6d41iim4zdk06wpsl9xhn7s3vpjl";
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
          version = "0.1.0.5";
          sha256 = "0ml5pqp918k2zgpw10sjn0nca0ivzb871zxcg73samm1aypfrm8c";
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
          version = "0.2.1.0";
          sha256 = "04bpdmk3ma4fnylipg4hkq3jfkrw5f009vbns6vah0znawkpjhnh";
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
          sha256 = "1vzg9fi597dbrcbjsr71y47rvmhiih7lg5rjnb297fzdlbmj1w0z";
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
          sha256 = "0dxk2r32ajmmc05vaxcp0yw6vgv4lkbmh8jcshncn98xgsfbgw14";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "The error function, erf, and related functions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      errors = callPackage ({ base, mkDerivation, safe, stdenv, transformers, transformers-compat, unexceptionalio }:
      mkDerivation {
          pname = "errors";
          version = "2.1.3";
          sha256 = "1wadhhl3hx7f1k7lda50ymifs6472dzy0ygb6kvxy5ms5yfis6i0";
          libraryHaskellDepends = [
            base
            safe
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
          sha256 = "1180l4z2cdgc6zj9pcr2c0lj28ka85kbk8sxd42fis65k2ahr61n";
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
          sha256 = "1gl7xzffsqmigam6zg0jsglncgzxqafld2p6kb7ccp9xirzdjsjd";
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
          sha256 = "1273nqws9ij1rp1bsq5jc7k2jxpqa0svawdbim05lf302y0firbc";
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
          version = "1.5.2";
          sha256 = "0qz0h2nckd0sqzx264q2j0dw66i5glp2vfih3wlm0a2kxcnw1p27";
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
          sha256 = "18nlj6xvnggy61gwbyrpmvbdkq928wv0wx2zcsljb52kbhddnp3d";
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
          sha256 = "13b7rrv8dw574k6lbl96nar67fx81058gvilsc42v0lgm38sbi6y";
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
          sha256 = "0k1frj6hpiij287sn91qvf1vms1b4zzs3xdq71xbam9cs80p5afy";
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
          sha256 = "04gpylngm2aalqcgdk7gy7jiw291dala1354spxa8wspxif94lgp";
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
          version = "0.1.0.1";
          sha256 = "0qypjnbkfayqyaymx8qrq4abddlrlzanf6lqhfn9cqzcgzr6735d";
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
          sha256 = "1d0jkzlhcvkikllnxz6ij8zsq6r4sx5ii3abahhdji1spkivvzaj";
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
          sha256 = "1w6x3kp3by5yjmam6wlrf9vap5l5rrqaip0djbrdp0fpf2imn30n";
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
          version = "0.1.5";
          sha256 = "1cg7mkhv3ip87952k8kcjl1gx1nvcbhbq71czhxlnzi00qg68jzg";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nikita-volkov/focus";
          description = "A general abstraction for manipulating elements of container data structures";
          license = stdenv.lib.licenses.mit;
        }) {};
      foldl = callPackage ({ base, bytestring, comonad, containers, contravariant, mkDerivation, mwc-random, primitive, profunctors, stdenv, text, transformers, vector }:
      mkDerivation {
          pname = "foldl";
          version = "1.2.4";
          sha256 = "1dg3gij627pfqzy2gc9mszlxbwklxqhdskx4hjhs0aj2faqpg5qw";
          libraryHaskellDepends = [
            base
            bytestring
            comonad
            containers
            contravariant
            mwc-random
            primitive
            profunctors
            text
            transformers
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
          sha256 = "0rrkydr0zdcwji6grnrm8mlxj67q08sh6vhfnxm35g6k6x0bfba3";
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
          version = "0.0.8";
          sha256 = "1fy9phm8jpdf15qfc9d9g2hj1bxp6dsvz4s6pv1kba4bfnaf5608";
          revision = "1";
          editedCabalFile = "0d7x7qm96jar5axqdca9p9da3m8fjjc8nv49sz4g1d2ypxckxdgh";
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
          sha256 = "1147s393442xf4gkpbq0rd1p286vmykgx85mxhk5d1c7wfm4bzn9";
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
          version = "0.2.1";
          sha256 = "0asl313a52qx2w6dw25g845683xsl840bwjh118nkwi5v1xipkzb";
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
          sha256 = "1imw36k5kxfl7ik0mzjxa8xzqg6hs3k253kpi19a9l53wxa0mwv9";
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
          sha256 = "1y92q4dmbyc24hjjvq02474s9grwabxffn16y31gzaqhm0m0z5i9";
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
          version = "0.2.5.0";
          sha256 = "1p2dsdjxl1ld40c890i4jagp48zxp3i2njr9jd9ma89ydkypr5zk";
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
          sha256 = "1w7qkgwpbp5h0hm8p2b5bbysyvnjrqbkqkfzd4ngz0yxy9qy402x";
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
          sha256 = "1cnn5gcwnc711ngx5hac3x2s4f6dkdl7li5pc3c02lcghpqf9fs4";
          libraryHaskellDepends = [ rts ];
          doHaddock = false;
          doCheck = false;
          description = "GHC primitives";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      gitrev = callPackage ({ base, directory, filepath, mkDerivation, process, stdenv, template-haskell }:
      mkDerivation {
          pname = "gitrev";
          version = "1.2.0";
          sha256 = "00ii00j5bnxnhnmzcsbqfin8kdj6n9ll7akg3j8apajwvd7f74a3";
          libraryHaskellDepends = [
            base
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
          sha256 = "1z453is01v0rnxlv6xx4iyaqv5vrp3bpz829mpv1a341sck2135h";
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
          sha256 = "1kp8h3617cimya8nnadljyy4vk66dzl5nzfm900k2gh3ci8kja6k";
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
          sha256 = "0p4sb7vv9cljv48wlx65wgdnkryrk5d6yfh7g4yrm20w1p449hl5";
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
          version = "1.19.5";
          sha256 = "1nj353q4z1g186fpjzf0dnsg71qhxqpamx8jy89rjjvv3p0kmw32";
          revision = "2";
          editedCabalFile = "1dvhv94lzmya938i5crshg9qbx7dxvkyxkhfbqimxkydxn7l2w7w";
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
          homepage = "http://www.haskell.org/happy/";
          description = "Happy is a parser generator for Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      hashable = callPackage ({ base, bytestring, deepseq, ghc-prim, integer-gmp, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "hashable";
          version = "1.2.6.0";
          sha256 = "0lhadvg4l18iff2hg4d5akn5f3lrg9pfwxpkn1j2zxbsh8y6d6s2";
          revision = "2";
          editedCabalFile = "11s0194vd8qjn2qmn75kvnqy1c5mh9s5lhfalys30vyl7rk4ai0q";
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
          version = "0.3.4.1";
          sha256 = "1y9g3hkwxqy38js24954yprbhryv7bpa7xhxwv7fhnc4bc4nf9bw";
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
          sha256 = "1v31xiaivrrn0q2jz8919wvkjplv1kxna5ajhsj701fqxm1i5vhj";
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
          sha256 = "16fzql0s34my9k1ib4rdjf9fhhijkmmbrvi148f865m51160wj7j";
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
          sha256 = "104d1yd84hclprg740nkz60vx589mnm094zriw6zczbgg8nkclym";
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
          version = "1.24.1";
          sha256 = "1j3rpzjygh3igvnd1n2xn63bq68rs047cjxr2qi6xyfnivgf6vz4";
          isLibrary = true;
          isExecutable = true;
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
          version = "2.4.3";
          sha256 = "0dvfmzys2vcgaghmqdmq91j416vn556scdyx96gy0q8l8ziqhwrs";
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
          version = "2.4.3";
          sha256 = "0mg1144azwhrvk6224qnn7gbjyqlpq4kbxqns0hh4gwvg4s6z7bw";
          revision = "1";
          editedCabalFile = "0shqhsss67lhp2kn7spjn9ngfhlf6cnsrn66s6h1wk4f9k24lf5v";
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
          version = "2.4.3";
          sha256 = "0kmld0l61xr3qyjx2b2c61n5w1axy53ybbxnvhh404yxj747agda";
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
          sha256 = "1vxl9zazbaapijr6zmcj72j9wf7ka1pirrjbwddwwddg3zm0g5l1";
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
      http-api-data = callPackage ({ Cabal, attoparsec, attoparsec-iso8601, base, bytestring, containers, directory, filepath, hashable, http-types, mkDerivation, stdenv, text, time, time-locale-compat, unordered-containers, uri-bytestring, uuid-types }:
      mkDerivation {
          pname = "http-api-data";
          version = "0.3.7";
          sha256 = "1ah9lfandgb14zds9sivvwrnwd4gcwril38qw4dann4rilsdilnh";
          setupHaskellDepends = [
            base
            Cabal
            directory
            filepath
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
          version = "0.5.6.1";
          sha256 = "1v9bdb8dkhb5g6jl9azk86ig7ia8xh9arr64n7s8r94fp0vl6c1c";
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
          version = "0.3.4.1";
          sha256 = "1mbwdfn4hs8lcwml2l6xv4n068l9zlasyv6vwb2ylgm030pyv3xh";
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
          version = "2.2.3.1";
          sha256 = "03na2nbm9la0shlijvjyb5mpp1prfskk4jmjy8iz707r0731dbjk";
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
          sha256 = "0dknh28kyarnzqrsc80ssalxjrq0qbv7ir49247p2grb7rh0dqgj";
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
          sha256 = "1ly93k3d6kilma8gv6y1vf4d3lz4xg5xwi5p8x10w9al13sjqxpg";
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
          sha256 = "0l7mnvqyppxpnq6ds4a9f395zdbl22z3sxiry1myfs8wvj669vbv";
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
      http2 = callPackage ({ array, base, bytestring, bytestring-builder, case-insensitive, containers, mkDerivation, psqueues, stdenv, stm }:
      mkDerivation {
          pname = "http2";
          version = "1.6.3";
          sha256 = "0hww0rfsv6lqx62qzycbcqy5q6rh9k09qkyjkdm5m1sp1z50wqk1";
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
          sha256 = "1612f455dw37da9g7bsd1s5kyi84mnr1ifnjw69892amyimi47fp";
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
          sha256 = "08f1qcp57aj5mjy26dl3bi3lcg0p8ylm0qw4c6zbc1vhgnmxl4gg";
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
          version = "1.0.1";
          sha256 = "0k3q79yjwln3fk0m1mwsxc3rypysx6ayl13xqgm254dip273yi8g";
          revision = "1";
          editedCabalFile = "1kk94f88qnmvwya9afpr4gqygvg02qc8m571hvd6fmwgsfvphv1y";
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
      intervals = callPackage ({ array, base, distributive, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "intervals";
          version = "0.7.2";
          sha256 = "0a7ysncmwkqh7q1d8y1h4lb5373k93xdly3bqrjb7ihazmylml0d";
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
      io-streams = callPackage ({ attoparsec, base, bytestring, bytestring-builder, mkDerivation, network, primitive, process, stdenv, text, time, transformers, vector, zlib-bindings }:
      mkDerivation {
          pname = "io-streams";
          version = "1.3.6.1";
          sha256 = "0a1nr29qg5z0fqjnivzzy69bfxv7r9aw9yf2i53alcmiqjmx9p18";
          libraryHaskellDepends = [
            attoparsec
            base
            bytestring
            bytestring-builder
            network
            primitive
            process
            text
            time
            transformers
            vector
            zlib-bindings
          ];
          doHaddock = false;
          doCheck = false;
          description = "Simple, composable, and easy-to-use stream I/O";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      io-streams-haproxy = callPackage ({ attoparsec, base, bytestring, io-streams, mkDerivation, network, stdenv, transformers }:
      mkDerivation {
          pname = "io-streams-haproxy";
          version = "1.0.0.1";
          sha256 = "0zwjdsg1pcxzd8s0d308q4jhx0pfrk2aq8q039gs8k9y8h9cbh64";
          revision = "2";
          editedCabalFile = "1zm580jcncmh667k51k47xwwhd171r3f0h00d25hi6isq812ia40";
          libraryHaskellDepends = [
            attoparsec
            base
            bytestring
            io-streams
            network
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://snapframework.com/";
          description = "HAProxy protocol 1.5 support for io-streams";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      iproute = callPackage ({ appar, base, byteorder, containers, mkDerivation, network, stdenv }:
      mkDerivation {
          pname = "iproute";
          version = "1.7.1";
          sha256 = "1viyxq3m1aifl05w0hxwrhhhcfpmvwz4ymil2gngi4nfm0yd1f2p";
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
          sha256 = "1x2h54sx4ycik34q8f9g698xc2b7fai18918cd08qx7w7ny8nai1";
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
      kan-extensions = callPackage ({ adjunctions, array, base, comonad, containers, contravariant, distributive, free, mkDerivation, mtl, semigroupoids, stdenv, tagged, transformers }:
      mkDerivation {
          pname = "kan-extensions";
          version = "5.0.1";
          sha256 = "1qm0kf4krmyjbjynn96ab0h3q117vwcia5nin7n2b8b4f3jrzph1";
          libraryHaskellDepends = [
            adjunctions
            array
            base
            comonad
            containers
            contravariant
            distributive
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
      lens = callPackage ({ Cabal, array, base, base-orphans, bifunctors, bytestring, cabal-doctest, comonad, containers, contravariant, distributive, exceptions, filepath, free, ghc-prim, hashable, kan-extensions, mkDerivation, mtl, parallel, profunctors, reflection, semigroupoids, semigroups, stdenv, tagged, template-haskell, text, th-abstraction, transformers, transformers-compat, unordered-containers, vector, void }:
      mkDerivation {
          pname = "lens";
          version = "4.15.3";
          sha256 = "0znd63nkpdndpdgpvcwnqm31v4w2d1ipkj8lnnbsabbrhywknqd2";
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
      lifted-base = callPackage ({ base, mkDerivation, monad-control, stdenv, transformers-base }:
      mkDerivation {
          pname = "lifted-base";
          version = "0.2.3.10";
          sha256 = "1z149mwf839yc0l3islm485n6yfwxbdjfbwd8yi0vi3nn5hfaxz6";
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
          version = "1.20.6";
          sha256 = "1xcr36g6rr7k0vlzsh6szzxxrdi1f6vb9sxdilcpsb8xjvkk258m";
          revision = "1";
          editedCabalFile = "0jgfgn0i5882yhmbqc1w9g7gk713l5r66xyaxiwg7nmw82smmakk";
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
          version = "1";
          sha256 = "05ccx0l6rc97ls0jy7hfma5g0fa10s0h0kik1m596lk41776i6ji";
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
      log-warper = callPackage ({ aeson, ansi-terminal, base, containers, directory, dlist, errors, exceptions, extra, filepath, formatting, hashable, lens, mkDerivation, mmorph, monad-control, monad-loops, mtl, network, safecopy, stdenv, text, text-format, time, transformers, transformers-base, universum, unix, unordered-containers, yaml }:
      mkDerivation {
          pname = "log-warper";
          version = "1.1.2";
          sha256 = "0j17ylwga4vw0f0hahpmvm3nhk6s274m0msjv0r9jx6a6jx1wsmn";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            ansi-terminal
            base
            containers
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
          sha256 = "05knlckzx261yxbz38rqq8vy86zj1np0w2l32cnib6714vhaj5sz";
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
          sha256 = "1sv5vabsx332v1lpb6v3jv4zrzvpx1n7yprzd8wlcda5vsc5a6zp";
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
          version = "0.14.5";
          sha256 = "01d1bg8pkhw9mpyd7nm5zzpqv9kh9cj2fkd2ywvkay7np2r14820";
          revision = "1";
          editedCabalFile = "01g68h3r1h15rrbfsbbk0195fyn93x256j8xwzlb11074sdwr0gy";
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
          version = "0.4.8.0";
          sha256 = "1xbspqq1sgw6p16rwmdlwprjpcj2p0ppd1nn5iz3ynbifrqi42xa";
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
          version = "0.1.10.0";
          sha256 = "17dk2i7ggpipyjnb01wdlqcg4fnmgdbq7xhm34zaw97k03qc9pmi";
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
          sha256 = "1fg9cqpp5lswk8ajlq4f41n12c2v2naz179l8dsz6zisjqj4l5l3";
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
          sha256 = "0qs5alhy719a14lrs7rnh2qsn1146czg68gvgylf4m5jh4w7vwp1";
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
          version = "1.0.1.0";
          sha256 = "1x018gi5irznx5rgzmkr2nrgh26r8cvqwkcfc6n6y05pdjf21c6l";
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
          sha256 = "062c2sn3hc8h50p1mhqkpyv6x8dydz2zh3ridvlfjq9nqimszaky";
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
          sha256 = "0ldrzqy24fsszvn2a2nr77m2ih7xm0h9bgkjyv1l274aj18xyk7q";
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
          sha256 = "0bl4bd6jzdc5zm20q1g67ppkfh6j6yn8fwj6msjayj621cck67p2";
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
          version = "1.0.2";
          sha256 = "0crn1gd9jnf1j9n3dx9brw6dc4vfsydy0n3qs7hg49mp10ghl4da";
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
          sha256 = "07r86ip6jfa2ka84dpilap01g1pg8r5bqz2nk7js6mlnbh2lxzqk";
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
          sha256 = "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa";
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
          sha256 = "05j7yh0hh9nxic3dijmzv44kc6gzclvamdph7sq7w19wq57k6pq6";
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
          sha256 = "1by8xwjc23l6pa9l4iv7zp82dykpll3vc3hgxk0pgva724n8xhma";
          revision = "1";
          editedCabalFile = "1scwm1gs07znkj4ahfyxpwrksj4rdl1pa81xflcqhkqfgcndvgl3";
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
          sha256 = "0550dy0vwh81byi9bxhdzqx5y9lnvkwj5rbks5rbj2fylhyf8c2m";
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
          version = "2.6.3.1";
          sha256 = "1rl2gl37cf4k0ddsq93q15fwdz1l25nhl4w205krbh7d5dg5y12p";
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
          sha256 = "0xndvg776241fgjmynxfpy81f1csjmh8dg33yf0c8m71ychz3pzc";
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
          sha256 = "1w27zkvn39kjr9lmw9421y8w43h572ycsfafsb7kyvr3a4ihlgj2";
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
      newtype = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "newtype";
          version = "0.2";
          sha256 = "0ng4i5r73256gzwl6bw57h0abqixj783c3ggph1hk2wsplx0655p";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "A typeclass and set of functions for working with newtypes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      node-sketch = callPackage ({ aeson, async, attoparsec, base, binary, bytestring, containers, cryptonite, data-default, deepseq, ekg, ekg-core, exceptions, fetchgit, formatting, hashable, kademlia, lens, lifted-base, log-warper, mkDerivation, mmorph, monad-control, mtl, mwc-random, network, network-transport, network-transport-tcp, random, resourcet, semigroups, serokell-util, statistics, stdenv, stm, tagged, text, text-format, time, time-units, transformers, transformers-base, transformers-lift, universum, unordered-containers, vector }:
      mkDerivation {
          pname = "node-sketch";
          version = "0.2.0.0";
          src = fetchgit {
            url = "https://github.com/serokell/time-warp-nt.git";
            sha256 = "1lxxxdazgw1chrcjc8pqs3ac6xp699f2s70ymb2hgqq4xcfr1ic9";
            rev = "a6671797bd56b2f691b30a4950701d3591f8f925";
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
            ekg
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
            base
            binary
            bytestring
            containers
            network-transport-tcp
            random
            stm
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
          sha256 = "0l3viphiszvz5wqzg7a45zp40grwlab941q5ay29iyw8p3v8pbyv";
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
          sha256 = "1h9b26s3kfh2k0ih4383w90ibji6n0iwamxp6rfp2lbq1y5ibjqw";
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
          sha256 = "1x2abg2q9d26h1vzj40r6k7k3gqgappbs4g9d853vvg77837km4i";
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
          sha256 = "18kcjldpzay3k3309rvb9vqrp5b1gqp0hgymynqx7x2kgv7cz0sw";
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
          sha256 = "0zlcvxhx98k1akbv5fzsvwcrmb1rxsmmyaiwkhfrp5dxq6kg0is5";
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
          sha256 = "05rw8zhpqhx31zi6vg7zpyciaarh24j7g2p613xrpyrnksybjfrj";
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
          sha256 = "0vk7q9j2128q191zf1sg0ylj9s9djwayqk9747k0a5fin4f2b1vg";
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
          sha256 = "162sk5sg22w21wqz5qv8kx6ibxp99v5p20g3nknhm1kddk3hha1p";
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
          version = "4.3.3";
          sha256 = "0sgnlawi06mcwvm986vrixrmjdd3fy436kwfij8gn01vpa5p53bl";
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
      pipes-bytestring = callPackage ({ base, bytestring, mkDerivation, pipes, pipes-group, pipes-parse, stdenv, transformers }:
      mkDerivation {
          pname = "pipes-bytestring";
          version = "2.1.4";
          sha256 = "1svd8ssdqf8lp083g3lray823854i178hhn4ys3qhlxa53g74gvc";
          libraryHaskellDepends = [
            base
            bytestring
            pipes
            pipes-group
            pipes-parse
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
          version = "1.0.6";
          sha256 = "0rmpi9gb151gsmvx9f0q9vssd6fsf08ifxxynfp5jnv7lxmnzb87";
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
          sha256 = "0l7g184ksrh9qy8ixh49iv13amiwh40v6bbx0gcgq451knfl4n17";
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
          sha256 = "1a87q6l610rhxr23qfzzzif3zpfjhw3mg5gfcyjwqac25hdq73yj";
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
          sha256 = "0z560n3cfidp6d6my29vdkwqnga24pd0d6wp9kcmpp2kg3kcyhh2";
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
            sha256 = "1vhnyawq02cdbywlhbvpxwdwhmvff549zlq4yda9346cr27ap565";
            rev = "eedf6b1f6f6b931fa578cf886e36b8a3a9fcda0d";
          };
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
          sha256 = "05dya1vdvq29hkhkdlsglzhw7bdn51rvs1javs0q75nf99c66k7m";
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
          sha256 = "0xzqdf3nl2h0ra4gnslm1m1nsxlsgc0hh6ky3vn578vh11zhifq9";
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
          sha256 = "164p5ybgf72hfpd3zsn8qpdxipn1pc1nl775jvn0kiqwymwjcqrv";
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
          version = "0.6.1.0";
          sha256 = "1j1q7l21rdm8kfs93vibr3xwkkhqis181w2k6klfhx5g5skiywwk";
          revision = "1";
          editedCabalFile = "0gb8lcn6bd6ilfln7ah9jmqq6324vgkrgdsnz1qvlyj3bi2w5ivf";
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
          sha256 = "1szhlzsjfmn5sd7r68scawqxa6l2xh0lszffi92bmhqr1b9g8wsl";
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
          version = "5.2";
          sha256 = "1905xv9y2sx1iya0zlrx7nxhlwap5vn144nxg7s8zsj58xff59w7";
          revision = "1";
          editedCabalFile = "1q0zva60kqb560fr0ii0gm227sg6q7ddbhriv64l6nfv509vw32k";
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
          version = "0.2.2.3";
          sha256 = "1dd6xv1wjxj1xinx155b14hijw8fafrg4096srzdzj7xyqq7qxbd";
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
          version = "0.10.1.0";
          sha256 = "08v2b4n3zpbwdz8v41scjpvwha3xnk0g6vgd58ki98h1gyvr4pqs";
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
      pvss = callPackage ({ base, binary, bytestring, cryptonite, cryptonite-openssl, deepseq, hourglass, integer-gmp, memory, mkDerivation, stdenv }:
      mkDerivation {
          pname = "pvss";
          version = "0.1";
          sha256 = "16gwq23d7p34n23ydi82lxz7cjvwdc684s36915fb2hm8k60n57s";
          revision = "1";
          editedCabalFile = "03nx4w2b6i2wn4x4ggbizc4k9y5bkjq4ihli5ln8bs60slz84srd";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            binary
            bytestring
            cryptonite
            cryptonite-openssl
            deepseq
            integer-gmp
            memory
          ];
          executableHaskellDepends = [
            base
            cryptonite
            deepseq
            hourglass
            memory
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/input-output-hk/pvss-haskell#readme";
          description = "Public Verifiable Secret Sharing";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      quickcheck-instances = callPackage ({ QuickCheck, array, base, bytestring, containers, hashable, mkDerivation, old-time, scientific, stdenv, text, time, unordered-containers, vector }:
      mkDerivation {
          pname = "quickcheck-instances";
          version = "0.3.12";
          sha256 = "1wwvkzpams7i0j7nk5qj8vvhj8x5zcbgbgrpczszgvshva4bkmfx";
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
          version = "0.1.4";
          sha256 = "179qcy15yxgllsjc2czm2jsxaryfd6mcsr07ac43kc3i11cm0dvb";
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
          sha256 = "0nis3lbkp8vfx8pkr6v7b7kr5m334bzb0fk9vxqklnp2aw8a865p";
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
          sha256 = "0586bnlh0g2isc44jbjvafkcl4yw6lp1db8x6vr0pza0y08l8w2j";
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
      readable = callPackage ({ base, bytestring, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "readable";
          version = "0.3.1";
          sha256 = "1ja39cg26wy2fs00gi12x7iq5k8i366pbqi3p916skfa5jnkfc3h";
          libraryHaskellDepends = [
            base
            bytestring
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/mightybyte/readable";
          description = "Reading from Text and ByteString";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      reflection = callPackage ({ base, mkDerivation, stdenv, template-haskell }:
      mkDerivation {
          pname = "reflection";
          version = "2.1.2";
          sha256 = "0f9w0akbm6p8h7kzgcd2f6nnpw1wy84pqn45vfz1ch5j0hn8h2d9";
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
          sha256 = "0y1j4h2pg12c853nzmczs263di7xkkmlnsq5dlp5wgbgl49mgp10";
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
      regex-posix = callPackage ({ array, base, bytestring, containers, mkDerivation, regex-base, stdenv }:
      mkDerivation {
          pname = "regex-posix";
          version = "0.95.2";
          sha256 = "0gkhzhj8nvfn1ija31c7xnl6p0gadwii9ihyp219ck2arlhrj0an";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
            regex-base
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
          sha256 = "0f8x8wyr6m21g8dnxvnvalz5bsq37l125l6qhs0fscbvprsxc4nb";
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
          sha256 = "0090g6lgbdm9lywpqm2d3724nnnh24nx3vnlqr96qc2w486pmmrq";
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
          sha256 = "1x9f2qz57agl3xljp1wi0ab51p13czrpf6qjp3506rl9dg99j6as";
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
          sha256 = "0raipwawmah4h9ryja65b881dcj4yadrhh4c4718fdr0n89wgnzd";
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
          version = "0.3.14";
          sha256 = "13y8zlvifwwr5ybizqw0d1lzr763fnzlqsm8m5a1whpn933hqn6v";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ndmitchell/safe#readme";
          description = "Library of safe (exception free) functions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      safecopy = callPackage ({ array, base, bytestring, cereal, containers, mkDerivation, old-time, semigroups, stdenv, template-haskell, text, time, vector }:
      mkDerivation {
          pname = "safecopy";
          version = "0.9.3.1";
          sha256 = "06j7829q7j9vv4npzhjb04kismxacv06nwjbjd0482jcv0x9gvk3";
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
      scientific = callPackage ({ base, binary, bytestring, containers, deepseq, ghc-prim, hashable, integer-gmp, integer-logarithms, mkDerivation, stdenv, text, vector }:
      mkDerivation {
          pname = "scientific";
          version = "0.3.4.12";
          sha256 = "0pcm5s918sbyahbr7hinfkjcnv8fqp9xddkg6mmniyw2f1sqzyi6";
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
            text
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/scientific";
          description = "Numbers represented using scientific notation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      semigroupoids = callPackage ({ base, base-orphans, bifunctors, comonad, containers, contravariant, distributive, mkDerivation, semigroups, stdenv, tagged, transformers, transformers-compat }:
      mkDerivation {
          pname = "semigroupoids";
          version = "5.1";
          sha256 = "0dgqc59p4xx5cl8qkpm6sn4wd3n59rq7l6din76hf10bnklqrb0n";
          libraryHaskellDepends = [
            base
            base-orphans
            bifunctors
            comonad
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
          homepage = "http://github.com/ekmett/semigroupoids";
          description = "Semigroupoids: Category sans id";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      semigroups = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "semigroups";
          version = "0.18.2";
          sha256 = "1r6hsn3am3dpf4rprrj4m04d9318v9iq02bin0pl29dg4a3gzjax";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/semigroups/";
          description = "Anything that associates";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      serokell-util = callPackage ({ QuickCheck, acid-state, aeson, ansi-terminal, base, base16-bytestring, base64-bytestring, bytestring, clock, containers, deepseq, directory, exceptions, extra, filepath, formatting, hashable, lens, log-warper, mkDerivation, monad-control, mtl, optparse-applicative, parsec, quickcheck-instances, safecopy, scientific, semigroups, stdenv, stm, template-haskell, text, text-format, time-units, transformers, universum, unordered-containers, vector, yaml }:
      mkDerivation {
          pname = "serokell-util";
          version = "0.3";
          sha256 = "1crcc9vsg713cnsrpha54rki4a9pnls8sw4m9hz3pikmyqg01jsh";
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
          sha256 = "07ik9ddaj1vmq37dl4mg00rawa9phfapm8a52cs1b5km5fxaknp1";
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
          sha256 = "0ii60xn5khsj8w3glvwqpwrpd6v9yc1n52gk9qsfwfxq49x1rvch";
          revision = "5";
          editedCabalFile = "05zz0kvnmai230palf44f72gm1vadqyssk9hl4h0qq5263frbsli";
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
          sha256 = "16c1d618clq1mzgklls79xlkrh7mv17s3syc4ghg95qj87krhli8";
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
          sha256 = "0g87g48p179v1j3ki3vsvkk5gidqfp5yb9xwnh0j90v7x8ilvlcr";
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
      servant-swagger = callPackage ({ Cabal, QuickCheck, aeson, base, bytestring, cabal-doctest, directory, filepath, hspec, http-media, insert-ordered-containers, lens, mkDerivation, servant, stdenv, swagger2, text, unordered-containers }:
      mkDerivation {
          pname = "servant-swagger";
          version = "1.1.2.1";
          sha256 = "0qgrc01y9d2wsfg4r1iq71m2075qg75656wlljqb7pbkywxb0aih";
          revision = "2";
          editedCabalFile = "1hg56f50hw4m87h233qz7qm7mfn9a6kw1h1f0zcl2dfss7qrn8fa";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
            directory
            filepath
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
          version = "0.2.2.2.2.8";
          sha256 = "1yw483lfflpy1a16ybiy1ird1q6b0xhhaylyffxkxavaxdavrkvx";
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
          sha256 = "0cnbgrvb9byyahb37zlqrj05rj25v190crgcw8wmlgf0mwwxyn73";
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
      simple-sendfile = callPackage ({ base, bytestring, mkDerivation, network, stdenv, unix }:
      mkDerivation {
          pname = "simple-sendfile";
          version = "0.2.25";
          sha256 = "0k99j9xfcf83c55jmn202hdinhjaa4yn3dal4rvjk2w2rlhqirha";
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
      snap-core = callPackage ({ HUnit, attoparsec, base, bytestring, bytestring-builder, case-insensitive, containers, directory, filepath, hashable, io-streams, lifted-base, mkDerivation, monad-control, mtl, network, network-uri, old-locale, random, readable, regex-posix, stdenv, text, time, transformers, transformers-base, unix-compat, unordered-containers, vector }:
      mkDerivation {
          pname = "snap-core";
          version = "1.0.2.0";
          sha256 = "0p5nwm8b85h1b4q8lr955vhyw7l8xcfpkqbdkicrr6qn4x3vm3p4";
          revision = "1";
          editedCabalFile = "01bbi94sqilh8wr5nqd29vp21b62b9crbc3hcrbf65w53aba0h7x";
          libraryHaskellDepends = [
            attoparsec
            base
            bytestring
            bytestring-builder
            case-insensitive
            containers
            directory
            filepath
            hashable
            HUnit
            io-streams
            lifted-base
            monad-control
            mtl
            network
            network-uri
            old-locale
            random
            readable
            regex-posix
            text
            time
            transformers
            transformers-base
            unix-compat
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://snapframework.com/";
          description = "Snap: A Haskell Web Framework (core interfaces and types)";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      snap-server = callPackage ({ attoparsec, base, blaze-builder, bytestring, bytestring-builder, case-insensitive, clock, containers, filepath, io-streams, io-streams-haproxy, lifted-base, mkDerivation, mtl, network, old-locale, snap-core, stdenv, text, time, unix, unix-compat, vector }:
      mkDerivation {
          pname = "snap-server";
          version = "1.0.2.0";
          sha256 = "0jfmbc6dx4jl1jvx89v5ghw3q298wbdkzgmvnn1fxbiiadcjjzv7";
          revision = "1";
          editedCabalFile = "0ls5k20qdjh3bym66piwg7gvdaa47m5wqkgkd1spvv0y3cd1c95w";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            attoparsec
            base
            blaze-builder
            bytestring
            bytestring-builder
            case-insensitive
            clock
            containers
            filepath
            io-streams
            io-streams-haproxy
            lifted-base
            mtl
            network
            old-locale
            snap-core
            text
            time
            unix
            unix-compat
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://snapframework.com/";
          description = "A web server for the Snap Framework";
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
          sha256 = "0s689w1hh9g8ifl75xhzbv96ir07hwn04b4lgvbxzl8swa9ylir6";
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
          version = "0.2.3.1";
          sha256 = "12660m16c6sib3laaxn6w9s453pyx1b49myrmzfij372vq5bc5bn";
          revision = "1";
          editedCabalFile = "1kbf588dpzivh8fzrfgs761i4pqzcnpn8di7zxnq0ir9lwhfk2b0";
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
          sha256 = "0rjzx9iy5mx5igir6gvslznnx3gpxlb1xy1n8h4cn54cn3wxrspl";
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
          sha256 = "1vc12c3mnpspbycwkl0b22jqrdbg9fpmr1fxdxlmqwl603qy0zvf";
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
          sha256 = "111kpy1d6f5c0bggh6hyfm86q5p8bq1qbqf6dw2x4l4dxnar16cg";
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
          version = "0.2.15";
          sha256 = "1q5jsrmvmqlw6xjh6gj94qz1l0a37iybcqx42v17a50kpsy86925";
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
          sha256 = "0cla21v89gcvmr1iwzibq13v1yq02xg4h6k9l6kcprj7mhd5hcmi";
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
          sha256 = "1abxyjkn8xc8d33yhqxy1ki01kpzf4hy55f167qg4vk2ig5kh2p5";
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
          sha256 = "150rdank90h7v08x0wq4dffjbxv2daf5v9sqfs5mab76kinwxg26";
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
          sha256 = "0662m3i5xrdrr95w829bszkhp88mj9iy1zya54vk2sl5hz9wlmwp";
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
          sha256 = "1ap95xphqnrhv64c2a137wqslkdmb2jjd9ldb17gs1pw48k8hrl9";
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
          sha256 = "0jpy9xjcjdbpi3wk6mg7xwd7wfi2mma70p97v1ij5i8bj9qijpr9";
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
          sha256 = "0m7fpxh2viafcfm04xzy64jakx5c0p5wcam3bdwxgmj3r3q0qjc1";
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
      swagger2 = callPackage ({ aeson, base, base-compat, bytestring, containers, fetchgit, generics-sop, hashable, http-media, insert-ordered-containers, lens, mkDerivation, mtl, network, scientific, stdenv, template-haskell, text, time, transformers, unordered-containers, uuid-types, vector }:
      mkDerivation {
          pname = "swagger2";
          version = "2.1.3";
          src = fetchgit {
            url = "https://github.com/serokell/swagger2";
            sha256 = "00icvfppinysv0ig4jclp7fxpj993ay3zb5ibkz43idcrxv3yaja";
            rev = "6cc2063e1c8da9e701f3ac95549b8a33be9605de";
          };
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
          version = "0.6";
          sha256 = "1p3cnqjm13677r4a966zffzhi9b3a321aln8zs8ckqj0d9z1z3d3";
          revision = "1";
          editedCabalFile = "158ngdnlq9n1mil7cq2bzy4zkgx73zzms9q56wp6ll93m5mc4nlx";
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
          sha256 = "1yy5zsmmimhg6iaw9fmpwrxvxrgi5s6bfyqfihdsnx4bjvn7sp9l";
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
      tagged = callPackage ({ base, deepseq, mkDerivation, stdenv, template-haskell, transformers, transformers-compat }:
      mkDerivation {
          pname = "tagged";
          version = "0.8.5";
          sha256 = "16cdzh0bw16nvjnyyy5j9s60malhz4nnazw96vxb0xzdap4m2z74";
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
          sha256 = "18qq94j9bm91iswnxq2dm5dws5c7wm4k01q2rpf8py35cf3svnfq";
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
          sha256 = "171ngdd93i9prp9d5a4ix0alp30ahw2dvdk7i8in9mzscnv41csz";
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
          version = "1.2.0.4";
          sha256 = "0qk741yqnpd69sksgks2vb7zi50rglp9m498lzw4sh268a017rsi";
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
          homepage = "http://www.github.com/feuerbach/temporary";
          description = "Portable temporary file and directory support for Windows and Unix, based on code from Cabal";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      text = callPackage ({ array, base, binary, bytestring, deepseq, ghc-prim, integer-gmp, mkDerivation, stdenv }:
      mkDerivation {
          pname = "text";
          version = "1.2.2.1";
          sha256 = "0nrrzx0ws7pv4dx9jbc6jm2734al1cr0m6iwcnbck4v2yfyv3p8s";
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
          license = stdenv.lib.licenses.bsd3;
        }) {};
      text-format = callPackage ({ array, base, double-conversion, ghc-prim, integer-gmp, mkDerivation, old-locale, stdenv, text, time, transformers }:
      mkDerivation {
          pname = "text-format";
          version = "0.3.1.1";
          sha256 = "02zfgzfjvkaxbma1h2gr95h10c8q9gyaadag41q579j68iv15qbd";
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
          sha256 = "0445r2nns6009fmq0xbfpyv7jpzwv0snccjdg7hwj4xk4z0cwc1f";
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
          version = "0.2.2.0";
          sha256 = "00xnyknp1xgbr6rqbmdbpvxrnd3pw1wax46vv03g8bbjm0m4d7kd";
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
          sha256 = "17b73q0d5r8xixhvdp0hv4ap96l7s3f2y0j5cknp81b1hyinivlz";
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
          sha256 = "1dfb0z42vrmdx579lkam07ic03d3v5y19339a3ca0bwpprpzmihn";
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
          sha256 = "1f56cp6ckcalld5jchv0kxpjkwcsixd7smd0g7r8cg67ppx6m90x";
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
          version = "0.13.3";
          sha256 = "0vf4g2pwhgh242512cssbzsgbpfrnn1fj5kv7qw4wx8cdnvdb03k";
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
          version = "0.1.6";
          sha256 = "1b76zjxkj0v0n8zj9l0nwav2irm0c43rx6qjihfw8klmmxvx59df";
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
          sha256 = "1mki2s821b1zpdn5463qz5vl3kvxxam90iax1n6vznf0d7p4rik5";
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
          sha256 = "1jvzgifkalfypbm479fzxb7yi8d5z00b4y6hf6qjdlpl71pv8sgz";
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
          sha256 = "1vdcfr2hp9qh3ag90x6ikbdf42wiqpdylnplffna54bpnilbyi4i";
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
          version = "1.3.10";
          sha256 = "0w81bv1khibp36mnqb2685y01s3v1032l3qf1y6wx9fx807ps1cz";
          revision = "2";
          editedCabalFile = "17734zs69kph9pv8zpk4fm58gfhzcwjwqlk71sqib5r2zi0lby9h";
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
      transformers = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "transformers";
          version = "0.5.2.0";
          sha256 = "1qkhi8ssf8c4jnmrw9dzym3igqbzq7h48iisaykdfzdsm09qfh3c";
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
          sha256 = "11r3slgpgpra6zi2kjg3g60gvv17b1fh6qxipcpk8n86qx7lk8va";
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
          sha256 = "17yam0199fh9ndsn9n69jx9nvbsmymzzwbi23dck3dk4q57fz0fq";
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
          sha256 = "17g03r5hpnygx0c9ybr9za6208ay0cjvz47rkyplv1r9zcivzn0b";
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
          sha256 = "094nx29aahyrvbcn7yca9zs2a5rxz1is7510w1q43rpvza7hdjrg";
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
          sha256 = "0x0bshb13b7i4imn0pgpljcj109c9z5mgw84mjmlcg62d3ryvg6v";
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
      unbounded-delays = callPackage ({ Cabal, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "unbounded-delays";
          version = "0.1.0.10";
          sha256 = "08x1kk32qj0d4hba020cx9pzzrxv6r8zhv5564r32zr6gi1j2dhw";
          revision = "1";
          editedCabalFile = "1ip5c46w8v8ddv4hzxpl6zl892xy6803z49a24w4dg8pi5r4qhlq";
          setupHaskellDepends = [
            base
            Cabal
          ];
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
          sha256 = "1f7h7zwky4scdcyjspg4ksfh7x6yra0wjybxq70p7vcwpgk2nzlj";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/singpolyma/unexceptionalio";
          description = "IO without any non-error, synchronous exceptions";
          license = "unknown";
        }) {};
      universum = callPackage ({ base, bytestring, containers, deepseq, exceptions, ghc-prim, hashable, microlens, microlens-mtl, mkDerivation, mtl, safe, stdenv, stm, text, text-format, transformers, type-operators, unordered-containers, utf8-string, vector }:
      mkDerivation {
          pname = "universum";
          version = "0.4.3";
          sha256 = "17rrikfid54z8h95qns5q7bdxadnnggv1pl2d9ilz9pz9hi7a9g6";
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
          sha256 = "1709ip8k1vahy00zi7v7qccw6rr22qrf3vk54h97jxrnjiakc1gw";
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
          sha256 = "09vykw89x981fywy0w1pci2v8zy3ajyjwh9z2n610vjacmd1v03j";
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
          sha256 = "1qdlc9raih8s0m3x8x3n7q3ngh4faw2alv9l78sp6gnx648k0c8i";
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
          sha256 = "1a7flszhhgyjn0nm9w7cm26jbf6vyx9ij1iij4sl11pjkwsqi8d4";
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
          version = "0.2.3.1";
          sha256 = "0vdiy6z9r5idml6rjbf6h2y24as97j34spcrrwpvgj3nraw18a0x";
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
          sha256 = "0h7imvxkahiy8pzr8cpsimifdfvv18lizrb33k6mnq70rcx9w2zv";
          revision = "2";
          editedCabalFile = "1b97s9picjl689hcz8scinv7c8k5iaal1livqr0l1l8yc4h0imhr";
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
          sha256 = "09xhk42yhxvqmka0iqrv3338asncz8cap3j0ic0ps896f2581b6z";
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
          sha256 = "1zdka5jnm1h6k36w3nr647yf3b5lqb336g3fkprhd6san9x52xlj";
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
          sha256 = "1w36ldr5iv0yiqbvibm7b1c66hf1ps5rbassz348zmi113d8k4cy";
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
      vector = callPackage ({ base, deepseq, ghc-prim, mkDerivation, primitive, stdenv }:
      mkDerivation {
          pname = "vector";
          version = "0.11.0.0";
          sha256 = "1r1jlksy7b0kb0fy00g64isk6nyd9wzzdq31gx5v1wn38knj0lqa";
          revision = "2";
          editedCabalFile = "1kjafhgsyjqlvrpfv2vj17hipyv0zw56a2kbl6khzn5li9szvyib";
          libraryHaskellDepends = [
            base
            deepseq
            ghc-prim
            primitive
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/vector";
          description = "Efficient Arrays";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      vector-algorithms = callPackage ({ base, bytestring, mkDerivation, primitive, stdenv, vector }:
      mkDerivation {
          pname = "vector-algorithms";
          version = "0.7.0.1";
          sha256 = "0w4hf598lpxfg58rnimcqxrbnpqq2jmpjx82qa5md3q6r90hlipd";
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
          sha256 = "0niad09lbxz3cj20qllyj92lwbc013ihw4lby8fv07x5xjx5a4p1";
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
          sha256 = "0d82x55f5vvr1jvaia382m23rs690lg55pvavv8f4ph0y6kd91xy";
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
          sha256 = "0aygw0yb1h3yhmfl3bkwh5d3h0l4mmsxz7j53vdm6jryl1kgxzyk";
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
          sha256 = "08afasnirja21vr0bmzcywz4w29x736dmdv7h8nnh1l8bn7sd02x";
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
          sha256 = "0s36i0ca440l78d35isaam98z9x0dc0llx0ry48r901f3vrsq65k";
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
          sha256 = "0vkn5nws9vcjn809qv2jfhf9ckfcgvfhs1v3xx1b03iy0j59n215";
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
          version = "3.0.19.1";
          sha256 = "1xm744dmdajmvswr9wgzpkhb9jil2mkz4vzi96sqp1px692cmrzp";
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
          sha256 = "1w0b0vinsyqr37wciljkz8g5dcmfi2r210lq194a0wkycly9kkch";
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
          sha256 = "0ccasczm9x8sx6bpywd8ga3qji2rqkz1l2fy856qz7jdazmazgka";
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
          version = "3.2.11.2";
          sha256 = "0sjb9w7bjcl0ndx0994xj8v6gd396i2h6x57s1yb0xvpg2mn1r7v";
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
      warp-tls = callPackage ({ base, bytestring, cryptonite, data-default-class, mkDerivation, network, stdenv, streaming-commons, tls, wai, warp }:
      mkDerivation {
          pname = "warp-tls";
          version = "3.2.3";
          sha256 = "14m2bzk5ivz9gdpxlcj6qnh46f2lycm1ybdjnfkj2876zrqwii7m";
          libraryHaskellDepends = [
            base
            bytestring
            cryptonite
            data-default-class
            network
            streaming-commons
            tls
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
          sha256 = "1sx27ys3zwxjp8nafnkv0f38i5748cf5jv9kgn5944ird2k6zr9y";
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
          sha256 = "1k8ar8g03x3j95xisdqwzw2mjln7mpp3r2klvyhi5n853hhchq19";
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
          version = "0.1.2";
          sha256 = "1pbn8ra3qhwvw07p375cdmp7jzlg07hgdcr4cpscz3h7b9sy7fiw";
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
          sha256 = "0w2843z499d4nvx8jkq398rzp0zwqp4aydwqidpdrh2xdavv78v2";
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
          sha256 = "1bjarnjz4v07wnkaqn46mrhxvy2f9anq6aw6lq3cf4xlzlr2i8la";
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
          version = "1.6.5";
          sha256 = "10s77746vq3w06q66dy0pcis4dbvwf2wix59yaajgar39qhr8f5m";
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
          version = "1.6.2";
          sha256 = "0yw09nwkvr324qz4sc27c0p28bz2h6gns6lkaz9mz92mgqf2dza9";
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
          version = "1.6.4";
          sha256 = "0k7zc0xp7r6kqmi39rpiicvq78xb0pr2cq6q5s3kmmsshllg13nr";
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
          version = "1.6.5";
          sha256 = "190w1sr3w6w49v3yvqz4grb0v09ym4gll3n8bxwijvbvcybk3xyi";
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
          sha256 = "0g814lj7vaxvib2g3r734221k80k7ap9czv9hinifn8syals3l9j";
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
          version = "0.8.22";
          sha256 = "18xmv55v5vb6y6bnqgbkkyzybf33lzsbgz90vk5xmjh4mpnrlhkn";
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
          sha256 = "1fx2k2qmgm2dj3fkxx2ry945fpdn02d4dkihjxma21xgdiilxsz4";
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
      zlib-bindings = callPackage ({ base, bytestring, mkDerivation, stdenv, zlib }:
      mkDerivation {
          pname = "zlib-bindings";
          version = "0.1.1.5";
          sha256 = "02ciywlz4wdlymgc3jsnicz9kzvymjw1www2163gxidnz4wb8fy8";
          revision = "2";
          editedCabalFile = "0fq49694gqkab8m0vq4i879blswczwd66n7xh4r4gwiahf0ryvqc";
          libraryHaskellDepends = [
            base
            bytestring
            zlib
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/snapframework/zlib-bindings";
          description = "Low-level bindings to the zlib package";
          license = stdenv.lib.licenses.bsd3;
        }) {};
    };
in
compiler.override {
  initialPackages = stackPackages;
  configurationCommon = { ... }: self: super: {};
}

