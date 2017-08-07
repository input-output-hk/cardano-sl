{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc802
, ghc ? pkgs.haskell.compiler.ghc802
}:

with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let
  hackagePackages = import <nixpkgs/pkgs/development/haskell-modules/hackage-packages.nix>;
  stackPackages = { callPackage, pkgs, stdenv }:
self: {
      Cabal = callPackage ({ QuickCheck, array, base, binary, bytestring, containers, deepseq, directory, exceptions, filepath, mkDerivation, old-time, pretty, process, regex-posix, stdenv, tagged, tasty, tasty-hunit, tasty-quickcheck, time, transformers, unix }:
      mkDerivation {
          pname = "Cabal";
          version = "1.24.2.0";
          sha256 = "0h33v1716wkqh9wvq2wynvhwzkjjhg4aav0a1i3cmyq36n7fpl5p";
          revision = "1";
          editedCabalFile = "75fc1412e6e4c28de5fb44b8bab8393a1f98e4aa39da1abc4eba0aa56f02884b";
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
          testHaskellDepends = [
            base
            bytestring
            containers
            directory
            exceptions
            filepath
            old-time
            pretty
            process
            QuickCheck
            regex-posix
            tagged
            tasty
            tasty-hunit
            tasty-quickcheck
            transformers
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.haskell.org/cabal/";
          description = "A framework for packaging Haskell software";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      Glob = callPackage ({ HUnit, QuickCheck, base, containers, directory, dlist, filepath, mkDerivation, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, transformers, transformers-compat }:
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
          testHaskellDepends = [
            base
            containers
            directory
            dlist
            filepath
            HUnit
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://iki.fi/matti.niemenmaa/glob/";
          description = "Globbing library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      HUnit = callPackage ({ base, call-stack, deepseq, filepath, mkDerivation, stdenv }:
      mkDerivation {
          pname = "HUnit";
          version = "1.5.0.0";
          sha256 = "186ykl7vxlfgkd2k8k1rq7yzcryzjpqwmn4ci1nn9h6irqbivib5";
          libraryHaskellDepends = [
            base
            call-stack
            deepseq
          ];
          testHaskellDepends = [
            base
            call-stack
            deepseq
            filepath
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hspec/HUnit#readme";
          description = "A unit testing framework for Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      IfElse = callPackage ({ base, mkDerivation, mtl, stdenv }:
      mkDerivation {
          pname = "IfElse";
          version = "0.85";
          sha256 = "1kfx1bwfjczj93a8yqz1n8snqiq5655qgzwv1lrycry8wb1vzlwa";
          libraryHaskellDepends = [
            base
            mtl
          ];
          doHaddock = false;
          doCheck = false;
          description = "Anaphoric and miscellaneous useful control-flow";
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
      QuickCheck = callPackage ({ base, containers, mkDerivation, random, stdenv, template-haskell, test-framework, tf-random, transformers }:
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
          testHaskellDepends = [
            base
            containers
            template-haskell
            test-framework
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nick8325/quickcheck";
          description = "Automatic testing of Haskell programs";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      RSA = callPackage ({ DRBG, QuickCheck, SHA, base, binary, bytestring, crypto-api, crypto-pubkey-types, mkDerivation, pureMD5, stdenv, tagged, test-framework, test-framework-quickcheck2 }:
      mkDerivation {
          pname = "RSA";
          version = "2.2.0";
          sha256 = "1mzjlkw9i9r7r5a7mja01pq3ihvik2ncgah1jmznswaj6ga5cc19";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            crypto-api
            crypto-pubkey-types
            pureMD5
            SHA
          ];
          testHaskellDepends = [
            base
            binary
            bytestring
            crypto-api
            crypto-pubkey-types
            DRBG
            pureMD5
            QuickCheck
            SHA
            tagged
            test-framework
            test-framework-quickcheck2
          ];
          doHaddock = false;
          doCheck = false;
          description = "Implementation of RSA, using the padding schemes of PKCS#1 v2.1.";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      SHA = callPackage ({ QuickCheck, array, base, binary, bytestring, mkDerivation, stdenv, test-framework, test-framework-quickcheck2 }:
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
          testHaskellDepends = [
            array
            base
            binary
            bytestring
            QuickCheck
            test-framework
            test-framework-quickcheck2
          ];
          doHaddock = false;
          doCheck = false;
          description = "Implementations of the SHA suite of message digest functions";
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
      acid-state = callPackage ({ array, base, bytestring, cereal, containers, criterion, directory, extensible-exceptions, fetchgit, filepath, mkDerivation, mtl, network, random, safecopy, stdenv, stm, system-fileio, system-filepath, template-haskell, th-expand-syns, unix }:
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
          benchmarkHaskellDepends = [
            base
            criterion
            directory
            mtl
            random
            system-fileio
            system-filepath
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://acid-state.seize.it/";
          description = "Add ACID guarantees to any serializable Haskell data structure";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      adjunctions = callPackage ({ array, base, comonad, containers, contravariant, distributive, free, mkDerivation, mtl, profunctors, semigroupoids, semigroups, stdenv, tagged, transformers, transformers-compat, void }:
      mkDerivation {
          pname = "adjunctions";
          version = "4.3";
          sha256 = "1k1ykisf96i4g2zm47c45md7p42c4vsp9r73392pz1g8mx7s2j5r";
          revision = "1";
          editedCabalFile = "f88c4f5440736d64ad6a478e9feccc116727b5dc616fc6535cfe64ff75a2e980";
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
      aeson = callPackage ({ HUnit, QuickCheck, attoparsec, base, base-compat, base-orphans, base16-bytestring, bytestring, containers, deepseq, dlist, generic-deriving, ghc-prim, hashable, hashable-time, mkDerivation, quickcheck-instances, scientific, stdenv, tagged, template-haskell, test-framework, test-framework-hunit, test-framework-quickcheck2, text, time, time-locale-compat, unordered-containers, vector }:
      mkDerivation {
          pname = "aeson";
          version = "1.0.2.1";
          sha256 = "0rlhr225vb6apxw1m0jpnjpbcwb2ij30n6r41qyhd5lr1ax6z9p0";
          revision = "1";
          editedCabalFile = "cf848d5d07a3e6962d7a274d452c772bc1c413a0f9f2d5f112fdde4556a7d7f1";
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
          testHaskellDepends = [
            attoparsec
            base
            base-compat
            base-orphans
            base16-bytestring
            bytestring
            containers
            dlist
            generic-deriving
            ghc-prim
            hashable
            hashable-time
            HUnit
            QuickCheck
            quickcheck-instances
            scientific
            tagged
            template-haskell
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
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
      aeson-compat = callPackage ({ QuickCheck, aeson, attoparsec, base, base-compat, base-orphans, bytestring, containers, exceptions, hashable, mkDerivation, nats, quickcheck-instances, scientific, semigroups, stdenv, tagged, tasty, tasty-hunit, tasty-quickcheck, text, time, time-locale-compat, unordered-containers, vector }:
      mkDerivation {
          pname = "aeson-compat";
          version = "0.3.6";
          sha256 = "0hnifh46g218ih666gha3r0hp8bahcl9aj1rr4jqyw2gykcnb8vs";
          revision = "6";
          editedCabalFile = "4cff8e8279e84b02ab85046d48f5a8a751d8c26f878a14daa7988b79ee1578c3";
          libraryHaskellDepends = [
            aeson
            attoparsec
            base
            base-compat
            bytestring
            containers
            exceptions
            hashable
            nats
            scientific
            semigroups
            tagged
            text
            time
            time-locale-compat
            unordered-containers
            vector
          ];
          testHaskellDepends = [
            aeson
            attoparsec
            base
            base-compat
            base-orphans
            bytestring
            containers
            exceptions
            hashable
            nats
            QuickCheck
            quickcheck-instances
            scientific
            semigroups
            tagged
            tasty
            tasty-hunit
            tasty-quickcheck
            text
            time
            time-locale-compat
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/aeson-compat#readme";
          description = "Compatibility layer for aeson";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      aeson-extra = callPackage ({ aeson, aeson-compat, attoparsec, base, base-compat, bytestring, containers, exceptions, hashable, mkDerivation, parsec, quickcheck-instances, recursion-schemes, scientific, stdenv, tasty, tasty-hunit, tasty-quickcheck, template-haskell, text, these, time, time-parsers, unordered-containers, vector }:
      mkDerivation {
          pname = "aeson-extra";
          version = "0.4.0.0";
          sha256 = "1555mc2vq74i8ydxrvc9ci9kiajml3i7ha4i4f9c0s4bbzvfvv3q";
          revision = "3";
          editedCabalFile = "df84e5ff1e5b0ad03cdd8173b5d8b41da124bb07759ac9fe47a6664e2ed09787";
          libraryHaskellDepends = [
            aeson
            aeson-compat
            attoparsec
            base
            base-compat
            bytestring
            containers
            exceptions
            hashable
            parsec
            recursion-schemes
            scientific
            template-haskell
            text
            time
            time-parsers
            unordered-containers
            vector
          ];
          testHaskellDepends = [
            base
            containers
            quickcheck-instances
            tasty
            tasty-hunit
            tasty-quickcheck
            these
            time
            time-parsers
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/aeson-extra#readme";
          description = "Extra goodies for aeson";
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
      asn1-encoding = callPackage ({ asn1-types, base, bytestring, hourglass, mkDerivation, mtl, stdenv, tasty, tasty-quickcheck, text }:
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
          testHaskellDepends = [
            asn1-types
            base
            bytestring
            hourglass
            mtl
            tasty
            tasty-quickcheck
            text
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
      async = callPackage ({ HUnit, base, mkDerivation, stdenv, stm, test-framework, test-framework-hunit }:
      mkDerivation {
          pname = "async";
          version = "2.1.1.1";
          sha256 = "1qj4fp1ynwg0l453gmm27vgkzb5k5m2hzdlg5rdqi9kf8rqy90yd";
          libraryHaskellDepends = [
            base
            stm
          ];
          testHaskellDepends = [
            base
            HUnit
            test-framework
            test-framework-hunit
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/simonmar/async";
          description = "Run IO operations asynchronously and wait for their results";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      attoparsec = callPackage ({ QuickCheck, array, base, bytestring, case-insensitive, containers, criterion, deepseq, directory, filepath, ghc-prim, http-types, mkDerivation, parsec, quickcheck-unicode, scientific, stdenv, tasty, tasty-quickcheck, text, transformers, unordered-containers, vector }:
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
          testHaskellDepends = [
            array
            base
            bytestring
            deepseq
            QuickCheck
            quickcheck-unicode
            scientific
            tasty
            tasty-quickcheck
            text
            transformers
            vector
          ];
          benchmarkHaskellDepends = [
            array
            base
            bytestring
            case-insensitive
            containers
            criterion
            deepseq
            directory
            filepath
            ghc-prim
            http-types
            parsec
            scientific
            text
            transformers
            unordered-containers
            vector
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
      authenticate-oauth = callPackage ({ RSA, SHA, base, base64-bytestring, blaze-builder, bytestring, crypto-pubkey-types, data-default, http-client, http-types, mkDerivation, random, stdenv, time, transformers, transformers-compat }:
      mkDerivation {
          pname = "authenticate-oauth";
          version = "1.6";
          sha256 = "0xc37yql79r9idjfdhzg85syrwwgaxggcv86myi6zq2pzl89yvfj";
          revision = "1";
          editedCabalFile = "ba9e93e7b949fa4799ae7b3cb302df38dc7fb0be0460803b6c48636317b2bcbb";
          libraryHaskellDepends = [
            base
            base64-bytestring
            blaze-builder
            bytestring
            crypto-pubkey-types
            data-default
            http-client
            http-types
            random
            RSA
            SHA
            time
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/yesodweb/authenticate";
          description = "Library to authenticate with OAuth for Haskell web applications";
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
      base-compat = callPackage ({ QuickCheck, base, hspec, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "base-compat";
          version = "0.9.3";
          sha256 = "0452l6zf6fjhy4kxqwv6i6hhg6yfx4wcg450k3axpyj30l7jnq3x";
          libraryHaskellDepends = [
            base
            unix
          ];
          testHaskellDepends = [
            base
            hspec
            QuickCheck
          ];
          doHaddock = false;
          doCheck = false;
          description = "A compatibility layer for base";
          license = stdenv.lib.licenses.mit;
        }) {};
      base-orphans = callPackage ({ QuickCheck, base, ghc-prim, hspec, mkDerivation, stdenv }:
      mkDerivation {
          pname = "base-orphans";
          version = "0.5.4";
          sha256 = "0qv20n4yabg7sc3rs2dd46a53c7idnd88by7n3s36dkbc21m41q4";
          libraryHaskellDepends = [
            base
            ghc-prim
          ];
          testHaskellDepends = [
            base
            hspec
            QuickCheck
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
      base58-bytestring = callPackage ({ base, bytestring, criterion, mkDerivation, quickcheck-assertions, quickcheck-instances, stdenv, tasty, tasty-quickcheck }:
      mkDerivation {
          pname = "base58-bytestring";
          version = "0.1.0";
          sha256 = "1ls05nzswjr6aw0wwk3q7cpv1hf0lw7vk16a5khm6l21yfcgbny2";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          testHaskellDepends = [
            base
            bytestring
            quickcheck-assertions
            quickcheck-instances
            tasty
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://bitbucket.org/s9gf4ult/base58-bytestring";
          description = "Implementation of BASE58 transcoding for ByteStrings";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      base64-bytestring = callPackage ({ HUnit, QuickCheck, base, bytestring, containers, mkDerivation, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2 }:
      mkDerivation {
          pname = "base64-bytestring";
          version = "1.0.0.1";
          sha256 = "0l1v4ddjdsgi9nqzyzcxxj76rwar3lzx8gmwf2r54bqan3san9db";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          testHaskellDepends = [
            base
            bytestring
            containers
            HUnit
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/base64-bytestring";
          description = "Fast base64 encoding and decoding for ByteStrings";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      bifunctors = callPackage ({ QuickCheck, base, base-orphans, comonad, containers, hspec, mkDerivation, semigroups, stdenv, tagged, template-haskell, transformers, transformers-compat }:
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
          testHaskellDepends = [
            base
            hspec
            QuickCheck
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
      binary = callPackage ({ Cabal, HUnit, QuickCheck, array, attoparsec, base, bytestring, cereal, containers, criterion, deepseq, directory, filepath, mkDerivation, mtl, random, stdenv, tar, test-framework, test-framework-quickcheck2, unordered-containers, zlib }:
      mkDerivation {
          pname = "binary";
          version = "0.8.3.0";
          sha256 = "08d85qzna6zdkpgqwaw1d87biviv1b76zvk5qs3gg4kxwzfqa4r2";
          revision = "2";
          editedCabalFile = "d108ea136495c17b9fd3d22a66fd2152c25e0ae812aadc8814c7cb806fdae35b";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
          ];
          testHaskellDepends = [
            array
            base
            bytestring
            Cabal
            containers
            directory
            filepath
            HUnit
            QuickCheck
            random
            test-framework
            test-framework-quickcheck2
          ];
          benchmarkHaskellDepends = [
            array
            attoparsec
            base
            bytestring
            Cabal
            cereal
            containers
            criterion
            deepseq
            directory
            filepath
            mtl
            tar
            unordered-containers
            zlib
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/kolmodin/binary";
          description = "Binary serialisation for Haskell values using lazy ByteStrings";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      binary-conduit = callPackage ({ QuickCheck, base, binary, bytestring, conduit, hspec, mkDerivation, quickcheck-assertions, resourcet, stdenv, vector }:
      mkDerivation {
          pname = "binary-conduit";
          version = "1.2.4.1";
          sha256 = "10nalqf3zhg49b5drhw4y8zv9c3nsnlbc7bvw9la8vgzpihbnp24";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            conduit
            resourcet
            vector
          ];
          testHaskellDepends = [
            base
            binary
            bytestring
            conduit
            hspec
            QuickCheck
            quickcheck-assertions
            resourcet
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/qnikst/binary-conduit/";
          description = "data serialization/deserialization conduit library";
          license = stdenv.lib.licenses.mit;
        }) {};
      binary-orphans = callPackage ({ QuickCheck, aeson, base, binary, case-insensitive, hashable, mkDerivation, quickcheck-instances, scientific, stdenv, tagged, tasty, tasty-quickcheck, text, text-binary, time, unordered-containers, vector, vector-binary-instances }:
      mkDerivation {
          pname = "binary-orphans";
          version = "0.1.6.0";
          sha256 = "19c4avasgjzy81dg0ih4j769kqg0sn40jh6yxwjv5zh0bxzdrqg0";
          revision = "1";
          editedCabalFile = "b114cf269065159a2e49c71bf52245a0ba6c71fd623d2cee896ac2fd3a3dcbce";
          libraryHaskellDepends = [
            aeson
            base
            binary
            case-insensitive
            hashable
            scientific
            tagged
            text
            text-binary
            time
            unordered-containers
            vector
            vector-binary-instances
          ];
          testHaskellDepends = [
            aeson
            base
            binary
            case-insensitive
            hashable
            QuickCheck
            quickcheck-instances
            scientific
            tagged
            tasty
            tasty-quickcheck
            text
            time
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/binary-orphans#readme";
          description = "Orphan instances for binary";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      blaze-builder = callPackage ({ HUnit, QuickCheck, base, bytestring, deepseq, mkDerivation, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text, utf8-string }:
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
          testHaskellDepends = [
            base
            bytestring
            HUnit
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
            utf8-string
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/lpsmith/blaze-builder";
          description = "Efficient buffered output";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      blaze-html = callPackage ({ HUnit, QuickCheck, base, blaze-builder, blaze-markup, bytestring, containers, mkDerivation, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text }:
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
          testHaskellDepends = [
            base
            blaze-builder
            blaze-markup
            bytestring
            containers
            HUnit
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://jaspervdj.be/blaze";
          description = "A blazingly fast HTML combinator library for Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      blaze-markup = callPackage ({ HUnit, QuickCheck, base, blaze-builder, bytestring, containers, mkDerivation, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text }:
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
          testHaskellDepends = [
            base
            blaze-builder
            bytestring
            containers
            HUnit
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://jaspervdj.be/blaze";
          description = "A blazingly fast markup combinator library for Haskell";
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
      bytestring = callPackage ({ HUnit, QuickCheck, base, byteorder, deepseq, directory, dlist, ghc-prim, integer-gmp, mkDerivation, mtl, random, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2 }:
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
          testHaskellDepends = [
            base
            byteorder
            deepseq
            directory
            dlist
            ghc-prim
            HUnit
            mtl
            QuickCheck
            random
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
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
          editedCabalFile = "ceea8511e95a093c6fcb68c57bee629a9b479c50abc3f6b4981b296e72ac273c";
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
      call-stack = callPackage ({ base, mkDerivation, nanospec, stdenv }:
      mkDerivation {
          pname = "call-stack";
          version = "0.1.0";
          sha256 = "1qmihf5jafmc79sk52l6gpx75f5bnla2lp62kh3p34x3j84mwpzj";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            nanospec
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/sol/call-stack#readme";
          description = "Use GHC call-stacks in a backward compatible way";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-crypto = callPackage ({ base, bytestring, cryptonite, cryptonite-openssl, deepseq, fetchgit, hashable, memory, mkDerivation, stdenv, tasty, tasty-quickcheck }:
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
          testHaskellDepends = [
            base
            bytestring
            cryptonite
            memory
            tasty
            tasty-quickcheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/input-output-hk/cardano-crypto#readme";
          description = "Cryptography primitives for cardano";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-report-server = callPackage ({ HUnit, QuickCheck, aeson, aeson-pretty, base, bytestring, case-insensitive, directory, exceptions, fetchgit, filelock, filepath, formatting, hspec, http-types, lens, lifted-base, log-warper, mkDerivation, monad-control, mtl, network, optparse-applicative, optparse-simple, parsec, quickcheck-text, random, scotty, stdenv, text, time, transformers, universum, vector, wai, wai-extra, warp }:
      mkDerivation {
          pname = "cardano-report-server";
          version = "0.2.0";
          src = fetchgit {
            url = "https://github.com/input-output-hk/cardano-report-server.git";
            sha256 = "1rcspln6xn54mh65z0ayhivai6sc55xl72hh22l1g2y0j3xd9h54";
            rev = "b4fbd07c077b41c0e948bd377f1662b599f0bc4c";
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
            optparse-simple
            parsec
            random
            scotty
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
            optparse-simple
            parsec
            random
            scotty
            universum
            wai-extra
            warp
          ];
          testHaskellDepends = [
            aeson
            base
            hspec
            HUnit
            lens
            QuickCheck
            quickcheck-text
            text
            time
            transformers
            universum
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/input-output-hk/cardano-report-server";
          description = "Reporting server for CSL";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cardano-sl = callPackage ({ IfElse, QuickCheck, acid-state, aeson, ansi-terminal, ansi-wl-pprint, async, base, base58-bytestring, base64-bytestring, binary, binary-conduit, binary-orphans, bytestring, cardano-crypto, cardano-report-server, cardano-sl-core, cardano-sl-db, cardano-sl-godtossing, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, cardano-sl-txp, cardano-sl-update, cereal, conduit, containers, cpphs, criterion, cryptonite, cryptonite-openssl, data-default, deepseq, deriving-compat, digest, directory, dlist, ed25519, ekg, ekg-core, ekg-statsd, ether, exceptions, file-embed, filelock, filepath, focus, formatting, generic-arbitrary, gitrev, hashable, hashtables, hspec, http-client, http-client-tls, http-conduit, http-types, kademlia, lens, lifted-async, list-t, log-warper, lrucache, memory, mkDerivation, mmorph, monad-control, monad-loops, mono-traversable, mtl, neat-interpolation, network-info, network-transport, network-transport-tcp, node-sketch, optparse-applicative, optparse-simple, parsec, plutus-prototype, pvss, quickcheck-instances, random, reflection, regex-tdfa, regex-tdfa-text, resourcet, rocksdb-haskell, safecopy, serokell-util, servant-multipart, servant-server_0_10, servant_0_10, stdenv, stm, stm-containers, store, string-qq, tagged, template-haskell, temporary, text, text-format, th-lift-instances, time, time-units, transformers, transformers-base, transformers-lift, turtle, universum, unix, unordered-containers, vector, versions, wai, wai-extra, wai-websockets, warp, warp-tls, websockets, wreq, yaml }:
      mkDerivation {
          pname = "cardano-sl";
          version = "0.5.1";
          src = ./..;
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            acid-state
            aeson
            ansi-terminal
            async
            base
            base58-bytestring
            base64-bytestring
            binary
            binary-conduit
            binary-orphans
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
            cryptonite
            cryptonite-openssl
            data-default
            deepseq
            deriving-compat
            digest
            directory
            dlist
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
            IfElse
            kademlia
            lens
            lifted-async
            list-t
            log-warper
            lrucache
            memory
            mmorph
            monad-control
            monad-loops
            mono-traversable
            mtl
            neat-interpolation
            network-info
            network-transport-tcp
            node-sketch
            optparse-applicative
            optparse-simple
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
            servant_0_10
            servant-multipart
            servant-server_0_10
            stm
            stm-containers
            store
            tagged
            template-haskell
            temporary
            text
            text-format
            th-lift-instances
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            turtle
            universum
            unix
            unordered-containers
            vector
            versions
            wai
            wai-extra
            wai-websockets
            warp
            warp-tls
            websockets
            wreq
            yaml
          ];
          libraryToolDepends = [ cpphs ];
          executableHaskellDepends = [
            ansi-wl-pprint
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
            network-transport
            network-transport-tcp
            node-sketch
            optparse-simple
            parsec
            serokell-util
            stm-containers
            string-qq
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
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            cereal
            containers
            cryptonite
            data-default
            ether
            formatting
            generic-arbitrary
            hspec
            kademlia
            lens
            log-warper
            memory
            monad-control
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
            store
            tagged
            temporary
            text-format
            time-units
            transformers-base
            universum
            unordered-containers
            vector
          ];
          testToolDepends = [ cpphs ];
          benchmarkHaskellDepends = [
            base
            binary
            bytestring
            cardano-sl-core
            cardano-sl-txp
            containers
            criterion
            formatting
            hashtables
            lens
            log-warper
            QuickCheck
            serokell-util
            text-format
            universum
            vector
          ];
          benchmarkToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = false;
          description = "Cardano SL main implementation";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-core = callPackage ({ QuickCheck, aeson, autoexporter, base, base58-bytestring, binary, bytestring, cardano-crypto, cereal, concurrent-extra, containers, contravariant, cpphs, cryptonite, cryptonite-openssl, data-default, deepseq, deriving-compat, digest, directory, ed25519, ether, file-embed, filepath, formatting, generic-arbitrary, hashable, lens, log-warper, lrucache, memory, mkDerivation, mmorph, mtl, node-sketch, parsec, plutus-prototype, pvss, quickcheck-instances, random, reflection, resourcet, safecopy, semigroups, serokell-util, stdenv, stm, store, store-core, tagged, template-haskell, text, text-format, th-utilities, time, time-units, transformers, transformers-base, transformers-lift, universum, unordered-containers, vector, witherable, yaml }:
      mkDerivation {
          pname = "cardano-sl-core";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/core; echo source root reset to \$sourceRoot";
          libraryHaskellDepends = [
            aeson
            autoexporter
            base
            base58-bytestring
            binary
            bytestring
            cardano-crypto
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
            store
            store-core
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
            witherable
            yaml
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = false;
          description = "Cardano SL - core";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-db = callPackage ({ base, bytestring, cardano-sl-core, concurrent-extra, conduit, containers, cpphs, data-default, directory, ether, filepath, formatting, lens, log-warper, mkDerivation, mmorph, monad-control, mtl, node-sketch, resourcet, rocksdb-haskell, serokell-util, stdenv, text-format, transformers, transformers-base, transformers-lift, universum }:
      mkDerivation {
          pname = "cardano-sl-db";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/db; echo source root reset to \$sourceRoot";
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
          doCheck = false;
          description = "Cardano SL - basic DB interfaces";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-godtossing = callPackage ({ QuickCheck, aeson, base, bytestring, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, containers, cpphs, cryptonite, data-default, ether, file-embed, formatting, generic-arbitrary, lens, log-warper, mkDerivation, mmorph, mono-traversable, mtl, node-sketch, rocksdb-haskell, serokell-util, stdenv, stm, tagged, text, text-format, time-units, transformers, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-godtossing";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/godtossing; echo source root reset to \$sourceRoot";
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
          doCheck = false;
          description = "Cardano SL - GodTossing implementation of SSC";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-infra = callPackage ({ QuickCheck, aeson, base, base64-bytestring, binary, bytestring, cardano-report-server, cardano-sl-core, cardano-sl-db, containers, cpphs, data-default, directory, either, ether, exceptions, filepath, formatting, generic-arbitrary, hashable, kademlia, lens, list-t, log-warper, mkDerivation, mmorph, monad-control, mtl, network-info, network-transport, network-transport-tcp, node-sketch, optparse-simple, parsec, reflection, serokell-util, stdenv, stm, stm-containers, store, store-core, tagged, template-haskell, temporary, text, text-format, time, time-units, transformers, transformers-base, transformers-lift, universum, unordered-containers, wreq }:
      mkDerivation {
          pname = "cardano-sl-infra";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/infra; echo source root reset to \$sourceRoot";
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
            either
            ether
            exceptions
            filepath
            formatting
            generic-arbitrary
            hashable
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
            optparse-simple
            parsec
            QuickCheck
            reflection
            serokell-util
            stm
            stm-containers
            store
            store-core
            tagged
            template-haskell
            temporary
            text
            text-format
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unordered-containers
            wreq
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = false;
          description = "Cardano SL - infrastructural";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-lrc = callPackage ({ QuickCheck, base, bytestring, cardano-sl-core, cardano-sl-db, conduit, cpphs, ether, formatting, generic-arbitrary, lens, log-warper, mkDerivation, node-sketch, reflection, stdenv, text-format, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-lrc";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/lrc; echo source root reset to \$sourceRoot";
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
          doCheck = false;
          description = "Cardano SL - Leaders and Richmen computation";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-lwallet = callPackage ({ QuickCheck, acid-state, ansi-wl-pprint, base, base58-bytestring, binary, bytestring, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-update, containers, cpphs, data-default, dlist, either, ether, formatting, lens, lifted-async, log-warper, mkDerivation, mmorph, monad-control, monad-loops, mtl, network-transport-tcp, node-sketch, optparse-applicative, parsec, random, resourcet, safecopy, serokell-util, stdenv, stm, stm-containers, string-qq, tagged, text, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-lwallet";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/lwallet; echo source root reset to \$sourceRoot";
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
            lifted-async
            log-warper
            mmorph
            monad-control
            monad-loops
            mtl
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
            string-qq
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
          doCheck = false;
          description = "Cardano SL - Light wallet";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-ssc = callPackage ({ QuickCheck, base, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cpphs, cryptonite, ether, exceptions, formatting, lens, log-warper, memory, mkDerivation, mmorph, mtl, node-sketch, serokell-util, stdenv, stm, tagged, text-format, universum }:
      mkDerivation {
          pname = "cardano-sl-ssc";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/ssc; echo source root reset to \$sourceRoot";
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
          doCheck = false;
          description = "Cardano SL - the SSC class";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-tools = callPackage ({ Glob, QuickCheck, aeson, ansi-wl-pprint, array, async, attoparsec, base, bytestring, cardano-report-server, cardano-sl, cardano-sl-core, cardano-sl-infra, cardano-sl-txp, containers, cpphs, cryptonite, data-default, directory, ed25519, filepath, foldl, formatting, kademlia, lens, lifted-async, log-warper, mkDerivation, optparse-applicative, optparse-simple, optparse-text, parsec, process, purescript-bridge, random, random-shuffle, serokell-util, servant-multipart, servant-server_0_10, servant-swagger-ui, servant-swagger_1_1_2_1, servant_0_10, stdenv, stm, string-qq, swagger2, system-filepath, tar, text, time, time-units, turtle, universum, unordered-containers, vector, wreq }:
      mkDerivation {
          pname = "cardano-sl-tools";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/tools; echo source root reset to \$sourceRoot";
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
            cardano-sl-infra
            cardano-sl-txp
            containers
            cryptonite
            data-default
            directory
            ed25519
            filepath
            foldl
            formatting
            Glob
            kademlia
            lens
            lifted-async
            log-warper
            optparse-applicative
            optparse-simple
            optparse-text
            parsec
            process
            purescript-bridge
            QuickCheck
            random
            random-shuffle
            serokell-util
            servant_0_10
            servant-multipart
            servant-server_0_10
            servant-swagger_1_1_2_1
            servant-swagger-ui
            stm
            string-qq
            swagger2
            system-filepath
            tar
            text
            time
            time-units
            turtle
            universum
            unordered-containers
            vector
            wreq
          ];
          executableToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = false;
          description = "Cardano SL - Tools";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-txp = callPackage ({ QuickCheck, aeson, base, bytestring, cardano-sl-core, cardano-sl-db, cardano-sl-infra, conduit, containers, cpphs, data-default, ekg-core, ether, formatting, generic-arbitrary, hashable, lens, lifted-base, log-warper, mkDerivation, monad-control, mtl, neat-interpolation, node-sketch, plutus-prototype, resourcet, rocksdb-haskell, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, time-units, transformers, universum, unordered-containers, vector }:
      mkDerivation {
          pname = "cardano-sl-txp";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/txp; echo source root reset to \$sourceRoot";
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
          doCheck = false;
          description = "Cardano SL - transaction processing";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-update = callPackage ({ QuickCheck, aeson, base, binary, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, concurrent-extra, conduit, containers, cpphs, data-default, ether, exceptions, formatting, generic-arbitrary, hashable, lens, log-warper, mkDerivation, mtl, node-sketch, parsec, resourcet, rocksdb-haskell, safecopy, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, th-lift-instances, time-units, transformers, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-update";
          version = "0.5.1";
          src = ./..;
          postUnpack = "sourceRoot+=/update; echo source root reset to \$sourceRoot";
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
          doCheck = false;
          description = "Cardano SL - update";
          license = stdenv.lib.licenses.mit;
        }) {};
      case-insensitive = callPackage ({ HUnit, base, bytestring, criterion, deepseq, hashable, mkDerivation, stdenv, test-framework, test-framework-hunit, text }:
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
          testHaskellDepends = [
            base
            bytestring
            HUnit
            test-framework
            test-framework-hunit
            text
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
            deepseq
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/case-insensitive";
          description = "Case insensitive string comparison";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cassava = callPackage ({ HUnit, QuickCheck, array, attoparsec, base, blaze-builder, bytestring, containers, criterion, deepseq, hashable, lazy-csv, mkDerivation, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text, unordered-containers, vector }:
      mkDerivation {
          pname = "cassava";
          version = "0.4.5.1";
          sha256 = "17wxrwq977nyi225zlg3wj32f0ypyvikznhw59k0hxb4vkljlqkw";
          revision = "1";
          editedCabalFile = "c04d2ca56a5c725b3044ccf06c6fd09ff47265e3a1e125ae364363bfed2a0314";
          libraryHaskellDepends = [
            array
            attoparsec
            base
            blaze-builder
            bytestring
            containers
            deepseq
            hashable
            text
            unordered-containers
            vector
          ];
          testHaskellDepends = [
            attoparsec
            base
            bytestring
            hashable
            HUnit
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
            unordered-containers
            vector
          ];
          benchmarkHaskellDepends = [
            array
            attoparsec
            base
            blaze-builder
            bytestring
            containers
            criterion
            deepseq
            hashable
            lazy-csv
            text
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hvr/cassava";
          description = "A CSV parsing and encoding library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cereal = callPackage ({ QuickCheck, array, base, bytestring, containers, ghc-prim, mkDerivation, stdenv, test-framework, test-framework-quickcheck2 }:
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
          testHaskellDepends = [
            base
            bytestring
            QuickCheck
            test-framework
            test-framework-quickcheck2
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/GaloisInc/cereal";
          description = "A binary serialization library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cereal-vector = callPackage ({ QuickCheck, base, bytestring, cereal, mkDerivation, stdenv, vector }:
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
          testHaskellDepends = [
            base
            cereal
            QuickCheck
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/acfoltzer/cereal-vector";
          description = "Serialize instances for Data.Vector types.";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      charset = callPackage ({ array, base, bytestring, containers, mkDerivation, semigroups, stdenv, unordered-containers }:
      mkDerivation {
          pname = "charset";
          version = "0.3.7.1";
          sha256 = "1gn0m96qpjww8hpp2g1as5yy0wcwy4iq73h3kz6g0yxxhcl5sh9x";
          libraryHaskellDepends = [
            array
            base
            bytestring
            containers
            semigroups
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/charset";
          description = "Fast unicode character sets based on complemented PATRICIA tries";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      clock = callPackage ({ base, mkDerivation, stdenv, tasty, tasty-quickcheck }:
      mkDerivation {
          pname = "clock";
          version = "0.7.2";
          sha256 = "07v91s20halsqjmziqb1sqjp2sjpckl9by7y28aaklwqi2bh2rl8";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            tasty
            tasty-quickcheck
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
      code-page = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "code-page";
          version = "0.1.3";
          sha256 = "1491frk4jx6dlhifky9dvcxbsbcfssrz979a5hp5zn061rh8cp76";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [ base ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/RyanGlScott/code-page";
          description = "Windows code page library for Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      comonad = callPackage ({ Cabal, base, cabal-doctest, containers, contravariant, distributive, doctest, mkDerivation, semigroups, stdenv, tagged, transformers, transformers-compat }:
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
          testHaskellDepends = [
            base
            doctest
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/comonad/";
          description = "Comonads";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      concurrent-extra = callPackage ({ HUnit, async, base, mkDerivation, random, stdenv, stm, test-framework, test-framework-hunit, unbounded-delays }:
      mkDerivation {
          pname = "concurrent-extra";
          version = "0.7.0.10";
          sha256 = "04nw39pbfqa4ldymn706ij83hxa07c73r7hy18y5pwpmj05cq9vg";
          libraryHaskellDepends = [
            base
            stm
            unbounded-delays
          ];
          testHaskellDepends = [
            async
            base
            HUnit
            random
            stm
            test-framework
            test-framework-hunit
            unbounded-delays
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/concurrent-extra";
          description = "Extra concurrency primitives";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      conduit = callPackage ({ QuickCheck, base, containers, criterion, deepseq, exceptions, hspec, kan-extensions, lifted-base, mkDerivation, mmorph, monad-control, mtl, mwc-random, primitive, resourcet, safe, split, stdenv, transformers, transformers-base, vector }:
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
          testHaskellDepends = [
            base
            containers
            exceptions
            hspec
            mtl
            QuickCheck
            resourcet
            safe
            split
            transformers
          ];
          benchmarkHaskellDepends = [
            base
            containers
            criterion
            deepseq
            hspec
            kan-extensions
            mwc-random
            transformers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/snoyberg/conduit";
          description = "Streaming data processing library";
          license = stdenv.lib.licenses.mit;
        }) {};
      conduit-extra = callPackage ({ QuickCheck, async, attoparsec, base, blaze-builder, bytestring, bytestring-builder, conduit, criterion, directory, exceptions, filepath, hspec, mkDerivation, monad-control, network, primitive, process, resourcet, stdenv, stm, streaming-commons, text, transformers, transformers-base }:
      mkDerivation {
          pname = "conduit-extra";
          version = "1.1.15";
          sha256 = "13dvj271bhdaf83px99mlm0pgvc3474cmidh35jj775m1pmjkvvv";
          revision = "1";
          editedCabalFile = "94498d0883d567317ebd300ed3efcd1712ae0b444e35f50a941b3b62f57b164f";
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
          testHaskellDepends = [
            async
            attoparsec
            base
            blaze-builder
            bytestring
            bytestring-builder
            conduit
            directory
            exceptions
            hspec
            process
            QuickCheck
            resourcet
            stm
            streaming-commons
            text
            transformers
            transformers-base
          ];
          benchmarkHaskellDepends = [
            base
            blaze-builder
            bytestring
            bytestring-builder
            conduit
            criterion
            transformers
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
      constraints = callPackage ({ base, binary, deepseq, ghc-prim, hashable, mkDerivation, mtl, stdenv, transformers, transformers-compat }:
      mkDerivation {
          pname = "constraints";
          version = "0.9.1";
          sha256 = "11d76051i2a335bvack04dqvsz4zhgcms1jxlvy4a4c670l02vi7";
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
      containers = callPackage ({ ChasingBottoms, HUnit, QuickCheck, array, base, deepseq, ghc-prim, mkDerivation, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2 }:
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
          testHaskellDepends = [
            array
            base
            ChasingBottoms
            deepseq
            ghc-prim
            HUnit
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
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
      cookie = callPackage ({ HUnit, QuickCheck, base, blaze-builder, bytestring, data-default-class, deepseq, mkDerivation, old-locale, stdenv, tasty, tasty-hunit, tasty-quickcheck, text, time }:
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
          testHaskellDepends = [
            base
            blaze-builder
            bytestring
            HUnit
            QuickCheck
            tasty
            tasty-hunit
            tasty-quickcheck
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
      criterion = callPackage ({ Glob, HUnit, QuickCheck, aeson, ansi-wl-pprint, base, binary, bytestring, cassava, code-page, containers, deepseq, directory, filepath, hastache, js-flot, js-jquery, mkDerivation, mtl, mwc-random, optparse-applicative, parsec, statistics, stdenv, tasty, tasty-hunit, tasty-quickcheck, text, time, transformers, transformers-compat, vector, vector-algorithms }:
      mkDerivation {
          pname = "criterion";
          version = "1.1.4.0";
          sha256 = "0xps7jm8g1bg7a2y4b6mj5nhg3b595k5ysprf4711lwyfpy478jk";
          revision = "1";
          editedCabalFile = "61a5386463efe3da9c0bc5d14f6074e500dc76fc62e2dda40eaf81955716fe41";
          libraryHaskellDepends = [
            aeson
            ansi-wl-pprint
            base
            binary
            bytestring
            cassava
            code-page
            containers
            deepseq
            directory
            filepath
            Glob
            hastache
            js-flot
            js-jquery
            mtl
            mwc-random
            optparse-applicative
            parsec
            statistics
            text
            time
            transformers
            transformers-compat
            vector
            vector-algorithms
          ];
          testHaskellDepends = [
            aeson
            base
            bytestring
            HUnit
            QuickCheck
            statistics
            tasty
            tasty-hunit
            tasty-quickcheck
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.serpentine.com/criterion";
          description = "Robust, reliable performance measurement and analysis";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      crypto-api = callPackage ({ base, bytestring, cereal, entropy, mkDerivation, stdenv, tagged, transformers }:
      mkDerivation {
          pname = "crypto-api";
          version = "0.13.2";
          sha256 = "1vc27qcgbg7hf50rkqhlrs58zn1888ilh4b6wrrm07bnm48xacak";
          libraryHaskellDepends = [
            base
            bytestring
            cereal
            entropy
            tagged
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/TomMD/crypto-api";
          description = "A generic interface for cryptographic operations";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      crypto-pubkey-types = callPackage ({ asn1-encoding, asn1-types, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "crypto-pubkey-types";
          version = "0.4.3";
          sha256 = "0q0wlzjmpx536h1zcdzrpxjkvqw8abj8z0ci38138kpch4igbnby";
          libraryHaskellDepends = [
            asn1-encoding
            asn1-types
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-crypto-pubkey-types";
          description = "Generic cryptography Public keys algorithm types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cryptohash = callPackage ({ HUnit, QuickCheck, base, byteable, bytestring, criterion, cryptonite, ghc-prim, memory, mkDerivation, stdenv, tasty, tasty-hunit, tasty-quickcheck }:
      mkDerivation {
          pname = "cryptohash";
          version = "0.11.9";
          sha256 = "1yr2iyb779znj79j3fq4ky8l1y8a600a2x1fx9p5pmpwq5zq93y2";
          libraryHaskellDepends = [
            base
            byteable
            bytestring
            cryptonite
            ghc-prim
            memory
          ];
          testHaskellDepends = [
            base
            byteable
            bytestring
            HUnit
            QuickCheck
            tasty
            tasty-hunit
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [
            base
            byteable
            bytestring
            criterion
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-cryptohash";
          description = "collection of crypto hashes, fast, pure and practical";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cryptohash-md5 = callPackage ({ base, base16-bytestring, bytestring, criterion, mkDerivation, pureMD5, stdenv, tasty, tasty-hunit, tasty-quickcheck }:
      mkDerivation {
          pname = "cryptohash-md5";
          version = "0.11.100.1";
          sha256 = "1y8q7s2bn4gdknw1wjikdnar2b5pgz3nv3220lxrlgpsf23x82vi";
          revision = "1";
          editedCabalFile = "83170b82a6ca15da59f4f7831325128ce26e5ad00549d986fc294256ac963db7";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          testHaskellDepends = [
            base
            base16-bytestring
            bytestring
            pureMD5
            tasty
            tasty-hunit
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hvr/cryptohash-md5";
          description = "Fast, pure and practical MD5 implementation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cryptohash-sha1 = callPackage ({ SHA, base, base16-bytestring, bytestring, criterion, mkDerivation, stdenv, tasty, tasty-hunit, tasty-quickcheck }:
      mkDerivation {
          pname = "cryptohash-sha1";
          version = "0.11.100.1";
          sha256 = "1aqdxdhxhl9jldh951djpwxx8z7gzaqspxl7iwpl84i5ahrsyy9w";
          revision = "1";
          editedCabalFile = "0bd72d71afeb9183a7b9248499b871c31c2bd07166ffc97a220985ec6515f198";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          testHaskellDepends = [
            base
            base16-bytestring
            bytestring
            SHA
            tasty
            tasty-hunit
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hvr/cryptohash-sha1";
          description = "Fast, pure and practical SHA-1 implementation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cryptonite = callPackage ({ base, bytestring, criterion, deepseq, foundation, ghc-prim, integer-gmp, memory, mkDerivation, random, stdenv, tasty, tasty-hunit, tasty-kat, tasty-quickcheck }:
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
          testHaskellDepends = [
            base
            bytestring
            memory
            tasty
            tasty-hunit
            tasty-kat
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
            memory
            random
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-crypto/cryptonite";
          description = "Cryptography Primitives sink";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cryptonite-openssl = callPackage ({ base, bytestring, cryptonite, memory, mkDerivation, openssl, stdenv, tasty, tasty-hunit, tasty-kat, tasty-quickcheck }:
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
          testHaskellDepends = [
            base
            bytestring
            cryptonite
            tasty
            tasty-hunit
            tasty-kat
            tasty-quickcheck
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
      data-binary-ieee754 = callPackage ({ base, binary, mkDerivation, stdenv }:
      mkDerivation {
          pname = "data-binary-ieee754";
          version = "0.4.4";
          sha256 = "02nzg1barhqhpf4x26mpzvk7jd29nali033qy01adjplv2z5m5sr";
          libraryHaskellDepends = [
            base
            binary
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://john-millikin.com/software/data-binary-ieee754/";
          description = "Parser/Serialiser for IEEE-754 floating-point values";
          license = stdenv.lib.licenses.mit;
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
      data-msgpack = callPackage ({ QuickCheck, base, binary, bytestring, containers, criterion, data-binary-ieee754, deepseq, groom, hashable, hspec, mkDerivation, stdenv, text, unordered-containers, vector, void }:
      mkDerivation {
          pname = "data-msgpack";
          version = "0.0.9";
          sha256 = "0p2bn29z7cdfc10sd0f34qqhyswhg5rwnfdicnqprzpv3hziwas3";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            binary
            bytestring
            containers
            data-binary-ieee754
            deepseq
            hashable
            QuickCheck
            text
            unordered-containers
            vector
            void
          ];
          executableHaskellDepends = [
            base
            bytestring
            groom
          ];
          testHaskellDepends = [
            base
            bytestring
            containers
            hashable
            hspec
            QuickCheck
            text
            unordered-containers
            vector
            void
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
            deepseq
            QuickCheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://msgpack.org/";
          description = "A Haskell implementation of MessagePack";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      deepseq = callPackage ({ HUnit, array, base, mkDerivation, stdenv, test-framework, test-framework-hunit }:
      mkDerivation {
          pname = "deepseq";
          version = "1.4.2.0";
          sha256 = "0la9x4hvf1rbmxv8h9dk1qln21il3wydz6wbdviryh4h2wls22ny";
          libraryHaskellDepends = [
            array
            base
          ];
          testHaskellDepends = [
            array
            base
            HUnit
            test-framework
            test-framework-hunit
          ];
          doHaddock = false;
          doCheck = false;
          description = "Deep evaluation of data structures";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      deriving-compat = callPackage ({ QuickCheck, base, base-compat, base-orphans, containers, ghc-boot-th, ghc-prim, hspec, mkDerivation, stdenv, tagged, template-haskell, transformers, transformers-compat }:
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
          testHaskellDepends = [
            base
            base-compat
            base-orphans
            hspec
            QuickCheck
            tagged
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
          testHaskellDepends = [
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
      distributive = callPackage ({ Cabal, base, base-orphans, cabal-doctest, doctest, generic-deriving, hspec, mkDerivation, stdenv, tagged, transformers, transformers-compat }:
      mkDerivation {
          pname = "distributive";
          version = "0.5.2";
          sha256 = "1nbcyysnrkliy7xwx6f39p80kkp0vlvq14wdj6r0m5c1brmbxqmd";
          revision = "2";
          editedCabalFile = "29cf1ac04b774831a231c83cd13c4356c65dc657000f1a79ef3e42ad21e6e2f2";
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
          testHaskellDepends = [
            base
            doctest
            generic-deriving
            hspec
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/distributive/";
          description = "Distributive functors -- Dual to Traversable";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      dlist = callPackage ({ Cabal, QuickCheck, base, deepseq, mkDerivation, stdenv }:
      mkDerivation {
          pname = "dlist";
          version = "0.8.0.2";
          sha256 = "1ca1hvl5kd4api4gjyhwwavdx8snq6gf1jr6ab0zmjx7p77pwfbp";
          libraryHaskellDepends = [
            base
            deepseq
          ];
          testHaskellDepends = [
            base
            Cabal
            QuickCheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/spl/dlist";
          description = "Difference lists";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      double-conversion = callPackage ({ HUnit, base, bytestring, ghc-prim, mkDerivation, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text }:
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
          testHaskellDepends = [
            base
            bytestring
            HUnit
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/double-conversion";
          description = "Fast conversion between double precision floating point and text";
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
      ed25519 = callPackage ({ QuickCheck, base, bytestring, criterion, deepseq, directory, doctest, fetchgit, filemanip, filepath, ghc-prim, hlint, mkDerivation, stdenv }:
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
          testHaskellDepends = [
            base
            bytestring
            directory
            doctest
            filemanip
            filepath
            hlint
            QuickCheck
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
            deepseq
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
          editedCabalFile = "1a0e45e237b05447e891d7d781fe1c408b33babd15c9957118d0edccaa48e7d8";
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
          benchmarkHaskellDepends = [
            base
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
      entropy = callPackage ({ Cabal, base, bytestring, directory, filepath, mkDerivation, process, stdenv, unix }:
      mkDerivation {
          pname = "entropy";
          version = "0.3.7";
          sha256 = "1vzg9fi597dbrcbjsr71y47rvmhiih7lg5rjnb297fzdlbmj1w0z";
          revision = "1";
          editedCabalFile = "4f3e6690fc0835def94c8eedf0f0cfa5c54de6367cda8a7c908ce2bb18819e06";
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
      ether = callPackage ({ QuickCheck, base, criterion, deepseq, exceptions, ghc-prim, lens, mkDerivation, mmorph, monad-control, mtl, reflection, stdenv, tagged, tasty, tasty-quickcheck, template-haskell, transformers, transformers-base, transformers-lift, writer-cps-mtl }:
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
          testHaskellDepends = [
            base
            ghc-prim
            lens
            mtl
            QuickCheck
            tasty
            tasty-quickcheck
            transformers
          ];
          benchmarkHaskellDepends = [
            base
            criterion
            deepseq
            lens
            mtl
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://int-index.github.io/ether/";
          description = "Monad transformers and classes";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      exceptions = callPackage ({ QuickCheck, base, mkDerivation, mtl, stdenv, stm, template-haskell, test-framework, test-framework-quickcheck2, transformers, transformers-compat }:
      mkDerivation {
          pname = "exceptions";
          version = "0.8.3";
          sha256 = "1gl7xzffsqmigam6zg0jsglncgzxqafld2p6kb7ccp9xirzdjsjd";
          revision = "2";
          editedCabalFile = "dc2b4ed2a3de646d8ff599ff972e25b3a1a5165ead3a46ff84a3d443814c85ee";
          libraryHaskellDepends = [
            base
            mtl
            stm
            template-haskell
            transformers
            transformers-compat
          ];
          testHaskellDepends = [
            base
            mtl
            QuickCheck
            stm
            template-haskell
            test-framework
            test-framework-quickcheck2
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
      extra = callPackage ({ QuickCheck, base, clock, directory, filepath, mkDerivation, process, stdenv, time, unix }:
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
          testHaskellDepends = [
            base
            clock
            directory
            filepath
            QuickCheck
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
      fast-logger = callPackage ({ array, auto-update, base, bytestring, directory, easy-file, filepath, hspec, mkDerivation, stdenv, text, unix, unix-time }:
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
          testHaskellDepends = [
            base
            bytestring
            directory
            hspec
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/kazu-yamamoto/logger";
          description = "A fast logging system";
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
          testHaskellDepends = [
            base
            filepath
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
      filepath = callPackage ({ QuickCheck, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "filepath";
          version = "1.4.1.1";
          sha256 = "1d0jkzlhcvkikllnxz6ij8zsq6r4sx5ii3abahhdji1spkivvzaj";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            QuickCheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/filepath#readme";
          description = "Library for manipulating FilePaths in a cross platform way";
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
      foldl = callPackage ({ base, bytestring, comonad, containers, contravariant, criterion, mkDerivation, mwc-random, primitive, profunctors, stdenv, text, transformers, vector }:
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
          benchmarkHaskellDepends = [
            base
            criterion
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
      foundation = callPackage ({ QuickCheck, base, criterion, ghc-prim, mkDerivation, mtl, stdenv, tasty, tasty-hunit, tasty-quickcheck }:
      mkDerivation {
          pname = "foundation";
          version = "0.0.8";
          sha256 = "1fy9phm8jpdf15qfc9d9g2hj1bxp6dsvz4s6pv1kba4bfnaf5608";
          revision = "1";
          editedCabalFile = "f0b53e59bf5eb4f0c8d7896c8b98940ed5a15aba49b186bb2a5949932a3efd34";
          libraryHaskellDepends = [
            base
            ghc-prim
          ];
          testHaskellDepends = [
            base
            mtl
            QuickCheck
            tasty
            tasty-hunit
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [
            base
            criterion
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
      generic-deriving = callPackage ({ base, containers, ghc-prim, hspec, mkDerivation, stdenv, template-haskell }:
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
          testHaskellDepends = [
            base
            hspec
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
          testHaskellDepends = [ base ];
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
      groom = callPackage ({ base, haskell-src-exts, mkDerivation, stdenv }:
      mkDerivation {
          pname = "groom";
          version = "0.1.2.1";
          sha256 = "17g51p15209wwgq83clsd97xvy4kchbx8jzh74qgc9hvmz9s9d56";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            haskell-src-exts
          ];
          executableHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Pretty printing for well-behaved Show instances";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      hashable = callPackage ({ HUnit, QuickCheck, base, bytestring, criterion, deepseq, ghc-prim, integer-gmp, mkDerivation, random, siphash, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text, unix }:
      mkDerivation {
          pname = "hashable";
          version = "1.2.6.0";
          sha256 = "0lhadvg4l18iff2hg4d5akn5f3lrg9pfwxpkn1j2zxbsh8y6d6s2";
          revision = "2";
          editedCabalFile = "184445663ed46f30b4a7ca415a7482b5b0e0b1ddb31c5bb1b012a3b6490a4087";
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
          testHaskellDepends = [
            base
            bytestring
            ghc-prim
            HUnit
            QuickCheck
            random
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
            unix
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
            ghc-prim
            integer-gmp
            siphash
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/tibbe/hashable";
          description = "A class for types that can be converted to a hash value";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      haskell-src-exts = callPackage ({ array, base, containers, cpphs, directory, filepath, ghc-prim, happy, mkDerivation, mtl, pretty, pretty-show, smallcheck, stdenv, tasty, tasty-golden, tasty-smallcheck }:
      mkDerivation {
          pname = "haskell-src-exts";
          version = "1.18.2";
          sha256 = "0hq9f6r67gkhad4cc4dhahrwrz9kxfibhk8qrw5j0p7cvh23hn1i";
          libraryHaskellDepends = [
            array
            base
            cpphs
            ghc-prim
            pretty
          ];
          libraryToolDepends = [ happy ];
          testHaskellDepends = [
            base
            containers
            directory
            filepath
            mtl
            pretty-show
            smallcheck
            tasty
            tasty-golden
            tasty-smallcheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-suite/haskell-src-exts";
          description = "Manipulating Haskell source: abstract syntax, lexer, parser, and pretty-printer";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      hastache = callPackage ({ HUnit, base, blaze-builder, bytestring, containers, directory, filepath, ieee754, mkDerivation, mtl, process, stdenv, syb, text, transformers }:
      mkDerivation {
          pname = "hastache";
          version = "0.6.1";
          sha256 = "0r5l8k157pgvz1ck4lfid5x05f2s0nlmwf33f4fj09b1kmk8k3wc";
          revision = "5";
          editedCabalFile = "6e645296912c401a73a346c38a6ce2446d42591b3c602f7c657a626d9a0c8d3b";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            blaze-builder
            bytestring
            containers
            directory
            filepath
            ieee754
            mtl
            syb
            text
            transformers
          ];
          executableHaskellDepends = [
            base
            blaze-builder
            bytestring
            containers
            directory
            filepath
            ieee754
            mtl
            process
            syb
            text
            transformers
          ];
          testHaskellDepends = [
            base
            bytestring
            directory
            HUnit
            mtl
            syb
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/lymar/hastache";
          description = "Haskell implementation of Mustache templates";
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
      hostname = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "hostname";
          version = "1.0";
          sha256 = "0p6gm4328946qxc295zb6vhwhf07l1fma82vd0siylnsnsqxlhwv";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "A very simple package providing a cross-platform means of determining the hostname";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      hourglass = callPackage ({ base, bytestring, criterion, deepseq, mkDerivation, mtl, old-locale, stdenv, tasty, tasty-hunit, tasty-quickcheck, time }:
      mkDerivation {
          pname = "hourglass";
          version = "0.2.10";
          sha256 = "104d1yd84hclprg740nkz60vx589mnm094zriw6zczbgg8nkclym";
          libraryHaskellDepends = [
            base
            deepseq
          ];
          testHaskellDepends = [
            base
            deepseq
            mtl
            old-locale
            tasty
            tasty-hunit
            tasty-quickcheck
            time
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
            deepseq
            mtl
            old-locale
            time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/vincenthz/hs-hourglass";
          description = "simple performant time related library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      hspec = callPackage ({ HUnit, QuickCheck, base, call-stack, directory, hspec-core, hspec-discover, hspec-expectations, hspec-meta, mkDerivation, stdenv, stringbuilder, transformers }:
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
          testHaskellDepends = [
            base
            call-stack
            directory
            hspec-core
            hspec-discover
            hspec-expectations
            hspec-meta
            HUnit
            QuickCheck
            stringbuilder
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://hspec.github.io/";
          description = "A Testing Framework for Haskell";
          license = stdenv.lib.licenses.mit;
        }) {};
      hspec-core = callPackage ({ HUnit, QuickCheck, ansi-terminal, array, async, base, call-stack, deepseq, directory, filepath, hspec-expectations, hspec-meta, mkDerivation, process, quickcheck-io, random, setenv, silently, stdenv, temporary, tf-random, time, transformers }:
      mkDerivation {
          pname = "hspec-core";
          version = "2.4.3";
          sha256 = "0mg1144azwhrvk6224qnn7gbjyqlpq4kbxqns0hh4gwvg4s6z7bw";
          revision = "1";
          editedCabalFile = "bb384ac44c8e4c1ea0d1c6d8ac2d338e42f76cb2f2ea63a7b8901ea3b586186a";
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
          testHaskellDepends = [
            ansi-terminal
            array
            async
            base
            call-stack
            deepseq
            directory
            filepath
            hspec-expectations
            hspec-meta
            HUnit
            process
            QuickCheck
            quickcheck-io
            random
            setenv
            silently
            temporary
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
      hspec-discover = callPackage ({ base, directory, filepath, hspec-meta, mkDerivation, stdenv }:
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
          testHaskellDepends = [
            base
            directory
            filepath
            hspec-meta
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://hspec.github.io/";
          description = "Automatically discover and run Hspec tests";
          license = stdenv.lib.licenses.mit;
        }) {};
      hspec-expectations = callPackage ({ HUnit, base, call-stack, mkDerivation, nanospec, stdenv }:
      mkDerivation {
          pname = "hspec-expectations";
          version = "0.8.2";
          sha256 = "1vxl9zazbaapijr6zmcj72j9wf7ka1pirrjbwddwwddg3zm0g5l1";
          libraryHaskellDepends = [
            base
            call-stack
            HUnit
          ];
          testHaskellDepends = [
            base
            call-stack
            HUnit
            nanospec
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hspec/hspec-expectations#readme";
          description = "Catchy combinators for HUnit";
          license = stdenv.lib.licenses.mit;
        }) {};
      hspec-smallcheck = callPackage ({ QuickCheck, base, hspec, hspec-core, mkDerivation, smallcheck, stdenv }:
      mkDerivation {
          pname = "hspec-smallcheck";
          version = "0.4.2";
          sha256 = "1lsy71ri0lfvs6w1drwa4p69bcy0nrpb62dah3bg4vqwxfrd82ds";
          libraryHaskellDepends = [
            base
            hspec-core
            smallcheck
          ];
          testHaskellDepends = [
            base
            hspec
            hspec-core
            QuickCheck
            smallcheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://hspec.github.io/";
          description = "SmallCheck support for the Hspec testing framework";
          license = stdenv.lib.licenses.mit;
        }) {};
      http-api-data = callPackage ({ Cabal, HUnit, QuickCheck, attoparsec, attoparsec-iso8601, base, bytestring, containers, directory, doctest, filepath, hashable, hspec, http-types, mkDerivation, quickcheck-instances, stdenv, text, time, time-locale-compat, unordered-containers, uri-bytestring, uuid, uuid-types }:
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
          testHaskellDepends = [
            base
            bytestring
            directory
            doctest
            filepath
            hspec
            HUnit
            QuickCheck
            quickcheck-instances
            text
            time
            unordered-containers
            uuid
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/fizruk/http-api-data";
          description = "Converting to/from HTTP API data like URL pieces, headers and query parameters";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      http-client = callPackage ({ array, async, base, base64-bytestring, blaze-builder, bytestring, case-insensitive, containers, cookie, deepseq, directory, exceptions, filepath, ghc-prim, hspec, http-types, mime-types, mkDerivation, monad-control, network, network-uri, random, stdenv, streaming-commons, text, time, transformers, zlib }:
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
          testHaskellDepends = [
            async
            base
            base64-bytestring
            blaze-builder
            bytestring
            case-insensitive
            containers
            deepseq
            directory
            hspec
            http-types
            monad-control
            network
            network-uri
            streaming-commons
            text
            time
            transformers
            zlib
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/snoyberg/http-client";
          description = "An HTTP client engine";
          license = stdenv.lib.licenses.mit;
        }) {};
      http-client-tls = callPackage ({ base, bytestring, case-insensitive, connection, containers, criterion, cryptonite, data-default-class, exceptions, hspec, http-client, http-types, memory, mkDerivation, network, network-uri, stdenv, text, tls, transformers }:
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
          testHaskellDepends = [
            base
            hspec
            http-client
            http-types
          ];
          benchmarkHaskellDepends = [
            base
            criterion
            http-client
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/snoyberg/http-client";
          description = "http-client backend using the connection package and tls library";
          license = stdenv.lib.licenses.mit;
        }) {};
      http-conduit = callPackage ({ HUnit, aeson, base, blaze-builder, bytestring, case-insensitive, conduit, conduit-extra, connection, cookie, data-default-class, exceptions, hspec, http-client, http-client-tls, http-types, lifted-base, mkDerivation, monad-control, mtl, network, resourcet, stdenv, streaming-commons, temporary, text, time, transformers, utf8-string, wai, wai-conduit, warp, warp-tls }:
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
          testHaskellDepends = [
            aeson
            base
            blaze-builder
            bytestring
            case-insensitive
            conduit
            conduit-extra
            connection
            cookie
            data-default-class
            hspec
            http-client
            http-types
            HUnit
            lifted-base
            network
            resourcet
            streaming-commons
            temporary
            text
            time
            transformers
            utf8-string
            wai
            wai-conduit
            warp
            warp-tls
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.yesodweb.com/book/http-conduit";
          description = "HTTP client package with conduit interface and HTTPS support";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      http-date = callPackage ({ array, attoparsec, base, bytestring, doctest, hspec, mkDerivation, old-locale, stdenv, time }:
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
          testHaskellDepends = [
            base
            bytestring
            doctest
            hspec
            old-locale
            time
          ];
          doHaddock = false;
          doCheck = false;
          description = "HTTP Date parser/formatter";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      http-media = callPackage ({ QuickCheck, base, bytestring, case-insensitive, containers, mkDerivation, stdenv, test-framework, test-framework-quickcheck2 }:
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
          testHaskellDepends = [
            base
            bytestring
            case-insensitive
            containers
            QuickCheck
            test-framework
            test-framework-quickcheck2
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/zmthy/http-media";
          description = "Processing HTTP Content-Type and Accept headers";
          license = stdenv.lib.licenses.mit;
        }) {};
      http-types = callPackage ({ QuickCheck, array, base, blaze-builder, bytestring, case-insensitive, doctest, hspec, mkDerivation, quickcheck-instances, stdenv, text }:
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
          testHaskellDepends = [
            base
            blaze-builder
            bytestring
            doctest
            hspec
            QuickCheck
            quickcheck-instances
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/aristidb/http-types";
          description = "Generic HTTP types for Haskell (for both client and server code)";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      http2 = callPackage ({ Glob, aeson, aeson-pretty, array, base, bytestring, bytestring-builder, case-insensitive, containers, criterion, directory, doctest, filepath, hashtables, heaps, hex, hspec, mkDerivation, mwc-random, psqueues, stdenv, stm, text, unordered-containers, vector, word8 }:
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
          testHaskellDepends = [
            aeson
            aeson-pretty
            array
            base
            bytestring
            bytestring-builder
            case-insensitive
            containers
            directory
            doctest
            filepath
            Glob
            hex
            hspec
            psqueues
            stm
            text
            unordered-containers
            vector
            word8
          ];
          benchmarkHaskellDepends = [
            array
            base
            bytestring
            case-insensitive
            containers
            criterion
            hashtables
            heaps
            mwc-random
            psqueues
            stm
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/kazu-yamamoto/http2";
          description = "HTTP/2 library including frames, priority queues and HPACK";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      ieee754 = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "ieee754";
          version = "0.8.0";
          sha256 = "1lcs521g9lzy9d7337vg4w7q7s8500rfqy7rcifcz6pm6yfgyb8f";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/patperry/hs-ieee754";
          description = "Utilities for dealing with IEEE floating point numbers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      insert-ordered-containers = callPackage ({ QuickCheck, aeson, base, base-compat, hashable, lens, mkDerivation, semigroupoids, semigroups, stdenv, tasty, tasty-quickcheck, text, transformers, unordered-containers }:
      mkDerivation {
          pname = "insert-ordered-containers";
          version = "0.2.1.0";
          sha256 = "1612f455dw37da9g7bsd1s5kyi84mnr1ifnjw69892amyimi47fp";
          revision = "3";
          editedCabalFile = "6fdce987672b006226243aa17522b57ec7a9e1cab247802eddbdaa9dc5b06446";
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
          testHaskellDepends = [
            aeson
            base
            base-compat
            hashable
            lens
            QuickCheck
            semigroupoids
            semigroups
            tasty
            tasty-quickcheck
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
          editedCabalFile = "616d1775344a82a0ae1db1791fba719f4682a1ace908582ac4026db14231d4d5";
          libraryHaskellDepends = [
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          description = "Integer library based on GMP";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      integer-logarithms = callPackage ({ QuickCheck, array, base, ghc-prim, integer-gmp, mkDerivation, smallcheck, stdenv, tasty, tasty-hunit, tasty-quickcheck, tasty-smallcheck }:
      mkDerivation {
          pname = "integer-logarithms";
          version = "1.0.1";
          sha256 = "0k3q79yjwln3fk0m1mwsxc3rypysx6ayl13xqgm254dip273yi8g";
          revision = "1";
          editedCabalFile = "3e6c78b7d38f5767da86e1948a1816e0ede7f123f93a9594e7bb5a8c902369ce";
          libraryHaskellDepends = [
            array
            base
            ghc-prim
            integer-gmp
          ];
          testHaskellDepends = [
            base
            QuickCheck
            smallcheck
            tasty
            tasty-hunit
            tasty-quickcheck
            tasty-smallcheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/integer-logarithms";
          description = "Integer logarithms";
          license = stdenv.lib.licenses.mit;
        }) {};
      io-streams = callPackage ({ HUnit, QuickCheck, attoparsec, base, bytestring, bytestring-builder, deepseq, directory, filepath, mkDerivation, mtl, network, primitive, process, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text, time, transformers, vector, zlib, zlib-bindings }:
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
          testHaskellDepends = [
            attoparsec
            base
            bytestring
            bytestring-builder
            deepseq
            directory
            filepath
            HUnit
            mtl
            network
            primitive
            process
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
            time
            transformers
            vector
            zlib
            zlib-bindings
          ];
          doHaddock = false;
          doCheck = false;
          description = "Simple, composable, and easy-to-use stream I/O";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      io-streams-haproxy = callPackage ({ HUnit, attoparsec, base, bytestring, io-streams, mkDerivation, network, stdenv, test-framework, test-framework-hunit, transformers }:
      mkDerivation {
          pname = "io-streams-haproxy";
          version = "1.0.0.1";
          sha256 = "0zwjdsg1pcxzd8s0d308q4jhx0pfrk2aq8q039gs8k9y8h9cbh64";
          revision = "2";
          editedCabalFile = "80a82802c23a9a088b680040e0460e2734c8793f6486328f31b032cb2440a5fe";
          libraryHaskellDepends = [
            attoparsec
            base
            bytestring
            io-streams
            network
            transformers
          ];
          testHaskellDepends = [
            attoparsec
            base
            bytestring
            HUnit
            io-streams
            network
            test-framework
            test-framework-hunit
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://snapframework.com/";
          description = "HAProxy protocol 1.5 support for io-streams";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      iproute = callPackage ({ QuickCheck, appar, base, byteorder, containers, doctest, hspec, mkDerivation, network, safe, stdenv }:
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
          testHaskellDepends = [
            appar
            base
            byteorder
            containers
            doctest
            hspec
            network
            QuickCheck
            safe
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.mew.org/~kazu/proj/iproute/";
          description = "IP Routing Table";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      js-flot = callPackage ({ HTTP, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "js-flot";
          version = "0.8.3";
          sha256 = "0yjyzqh3qzhy5h3nql1fckw0gcfb0f4wj9pm85nafpfqp2kg58hv";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            HTTP
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ndmitchell/js-flot#readme";
          description = "Obtain minified flot code";
          license = stdenv.lib.licenses.mit;
        }) {};
      js-jquery = callPackage ({ HTTP, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "js-jquery";
          version = "3.1.1";
          sha256 = "011adwcf0rx57ld6c75m9rw90zd2qj0d4pf7rmdnf7fp5gbnfbyp";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            HTTP
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ndmitchell/js-jquery#readme";
          description = "Obtain minified jQuery code";
          license = stdenv.lib.licenses.mit;
        }) {};
      kademlia = callPackage ({ HUnit, MonadRandom, QuickCheck, base, binary, bytestring, containers, contravariant, cryptonite, data-default, errors, extra, fetchgit, memory, mkDerivation, mtl, network, quickcheck-instances, random, random-shuffle, stdenv, stm, store, tasty, tasty-hunit, tasty-quickcheck, time, transformers, transformers-compat }:
      mkDerivation {
          pname = "kademlia";
          version = "1.1.0.1";
          src = fetchgit {
            url = "https://github.com/serokell/kademlia.git";
            sha256 = "1m2lipmgyjmhc89ajzp5y360hqqwrq8k61fbmb1pm2c24wgn88is";
            rev = "bbdca50c263c6dae251e67eb36a7d4e1ba7c1cb6";
          };
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            bytestring
            containers
            contravariant
            cryptonite
            data-default
            errors
            extra
            memory
            MonadRandom
            mtl
            network
            random
            random-shuffle
            stm
            store
            time
            transformers
            transformers-compat
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
          testHaskellDepends = [
            base
            binary
            bytestring
            containers
            data-default
            errors
            extra
            HUnit
            MonadRandom
            mtl
            network
            QuickCheck
            quickcheck-instances
            random
            random-shuffle
            stm
            tasty
            tasty-hunit
            tasty-quickcheck
            time
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
      lens = callPackage ({ Cabal, HUnit, QuickCheck, array, base, base-orphans, bifunctors, bytestring, cabal-doctest, comonad, containers, contravariant, criterion, deepseq, directory, distributive, doctest, exceptions, filepath, free, generic-deriving, ghc-prim, hashable, hlint, kan-extensions, mkDerivation, mtl, nats, parallel, profunctors, reflection, semigroupoids, semigroups, simple-reflect, stdenv, tagged, template-haskell, test-framework, test-framework-hunit, test-framework-quickcheck2, test-framework-th, text, th-abstraction, transformers, transformers-compat, unordered-containers, vector, void }:
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
          testHaskellDepends = [
            base
            bytestring
            containers
            deepseq
            directory
            doctest
            filepath
            generic-deriving
            hlint
            HUnit
            mtl
            nats
            parallel
            QuickCheck
            semigroups
            simple-reflect
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            test-framework-th
            text
            transformers
            unordered-containers
            vector
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            comonad
            containers
            criterion
            deepseq
            generic-deriving
            transformers
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/lens/";
          description = "Lenses, Folds and Traversals";
          license = stdenv.lib.licenses.bsd2;
        }) {};
      lens-aeson = callPackage ({ Cabal, aeson, attoparsec, base, bytestring, cabal-doctest, doctest, generic-deriving, lens, mkDerivation, scientific, semigroups, simple-reflect, stdenv, text, unordered-containers, vector }:
      mkDerivation {
          pname = "lens-aeson";
          version = "1.0.1";
          sha256 = "1g8d6a7lhg6i1v56afm6x102pcxk0dxcyh2j7qqi62vp12kamljx";
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            aeson
            attoparsec
            base
            bytestring
            lens
            scientific
            text
            unordered-containers
            vector
          ];
          testHaskellDepends = [
            base
            doctest
            generic-deriving
            semigroups
            simple-reflect
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/lens/lens-aeson/";
          description = "Law-abiding lenses for aeson";
          license = stdenv.lib.licenses.mit;
        }) {};
      lifted-async = callPackage ({ HUnit, async, base, constraints, criterion, deepseq, lifted-base, mkDerivation, monad-control, mtl, stdenv, tasty, tasty-hunit, tasty-th, transformers-base }:
      mkDerivation {
          pname = "lifted-async";
          version = "0.9.1.1";
          sha256 = "0h4fskcgr053s21z43bh1mlz40i8f5d166d382frng3j6jw49b1i";
          libraryHaskellDepends = [
            async
            base
            constraints
            lifted-base
            monad-control
            transformers-base
          ];
          testHaskellDepends = [
            async
            base
            HUnit
            lifted-base
            monad-control
            mtl
            tasty
            tasty-hunit
            tasty-th
          ];
          benchmarkHaskellDepends = [
            async
            base
            criterion
            deepseq
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/maoe/lifted-async";
          description = "Run lifted IO operations asynchronously and wait for their results";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      lifted-base = callPackage ({ HUnit, base, criterion, mkDerivation, monad-control, monad-peel, stdenv, test-framework, test-framework-hunit, transformers, transformers-base, transformers-compat }:
      mkDerivation {
          pname = "lifted-base";
          version = "0.2.3.10";
          sha256 = "1z149mwf839yc0l3islm485n6yfwxbdjfbwd8yi0vi3nn5hfaxz6";
          libraryHaskellDepends = [
            base
            monad-control
            transformers-base
          ];
          testHaskellDepends = [
            base
            HUnit
            monad-control
            test-framework
            test-framework-hunit
            transformers
            transformers-base
            transformers-compat
          ];
          benchmarkHaskellDepends = [
            base
            criterion
            monad-control
            monad-peel
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/lifted-base";
          description = "lifted IO operations from the base library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      list-t = callPackage ({ HTF, base, base-prelude, mkDerivation, mmorph, monad-control, mtl, mtl-prelude, stdenv, transformers, transformers-base }:
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
          testHaskellDepends = [
            base-prelude
            HTF
            mmorph
            mtl-prelude
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nikita-volkov/list-t";
          description = "ListT done right";
          license = stdenv.lib.licenses.mit;
        }) {};
      log-warper = callPackage ({ HUnit, QuickCheck, aeson, ansi-terminal, async, base, containers, data-default, directory, dlist, errors, exceptions, extra, filepath, formatting, hashable, hspec, lens, mkDerivation, mmorph, monad-control, monad-loops, mtl, network, safecopy, stdenv, text, text-format, time, transformers, transformers-base, universum, unix, unordered-containers, yaml }:
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
          testHaskellDepends = [
            async
            base
            data-default
            directory
            filepath
            hspec
            HUnit
            lens
            QuickCheck
            universum
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/serokell/log-warper";
          description = "Flexible, configurable, monadic and pretty logging";
          license = stdenv.lib.licenses.mit;
        }) {};
      logict = callPackage ({ base, mkDerivation, mtl, stdenv }:
      mkDerivation {
          pname = "logict";
          version = "0.6.0.2";
          sha256 = "07hnirv6snnym2r7iijlfz00b60jpy2856zvqxh989q0in7bd0hi";
          libraryHaskellDepends = [
            base
            mtl
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://code.haskell.org/~dolio/";
          description = "A backtracking logic-programming monad";
          license = stdenv.lib.licenses.bsd3;
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
      managed = callPackage ({ base, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "managed";
          version = "1.0.5";
          sha256 = "1q3j8w6z8jaxh5a0ifzj51vipyhfrb1hk6l87pm319ysv91rkjdr";
          libraryHaskellDepends = [
            base
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          description = "A monad for managed values";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      math-functions = callPackage ({ HUnit, QuickCheck, base, deepseq, erf, mkDerivation, primitive, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, vector, vector-th-unbox }:
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
          testHaskellDepends = [
            base
            deepseq
            erf
            HUnit
            primitive
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            vector
            vector-th-unbox
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/math-functions";
          description = "Special functions and Chebyshev polynomials";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      megaparsec = callPackage ({ QuickCheck, base, bytestring, containers, criterion, deepseq, exceptions, hspec, hspec-expectations, mkDerivation, mtl, scientific, stdenv, text, transformers, weigh }:
      mkDerivation {
          pname = "megaparsec";
          version = "5.2.0";
          sha256 = "0204x5bklgvfydap1a2y76aicnjfs33jh786y7w6vsb54fpafl62";
          revision = "1";
          editedCabalFile = "6faae587ac65280ee855936319116bbc3015bd96eadb1a5ea107852fa5c905aa";
          libraryHaskellDepends = [
            base
            bytestring
            containers
            deepseq
            exceptions
            mtl
            QuickCheck
            scientific
            text
            transformers
          ];
          testHaskellDepends = [
            base
            bytestring
            containers
            exceptions
            hspec
            hspec-expectations
            mtl
            QuickCheck
            scientific
            text
            transformers
          ];
          benchmarkHaskellDepends = [
            base
            criterion
            deepseq
            weigh
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/mrkkrp/megaparsec";
          description = "Monadic parser combinators";
          license = stdenv.lib.licenses.bsd2;
        }) {};
      memory = callPackage ({ base, bytestring, deepseq, foundation, ghc-prim, mkDerivation, stdenv, tasty, tasty-hunit, tasty-quickcheck }:
      mkDerivation {
          pname = "memory";
          version = "0.14.5";
          sha256 = "01d1bg8pkhw9mpyd7nm5zzpqv9kh9cj2fkd2ywvkay7np2r14820";
          revision = "1";
          editedCabalFile = "fe81cc9b260784b0e8e71d4953441fc97a575200732ded56ce25c0900744e605";
          libraryHaskellDepends = [
            base
            bytestring
            deepseq
            foundation
            ghc-prim
          ];
          testHaskellDepends = [
            base
            tasty
            tasty-hunit
            tasty-quickcheck
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
          editedCabalFile = "4dd6d1966746918b7503dafa8b78b75df2245406baa083858e1a2310313aaef7";
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
      monad-loops = callPackage ({ base, mkDerivation, stdenv, tasty, tasty-hunit }:
      mkDerivation {
          pname = "monad-loops";
          version = "0.4.3";
          sha256 = "062c2sn3hc8h50p1mhqkpyv6x8dydz2zh3ridvlfjq9nqimszaky";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            tasty
            tasty-hunit
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/mokus0/monad-loops";
          description = "Monadic loops";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      monad-par = callPackage ({ HUnit, QuickCheck, abstract-deque, abstract-par, array, base, containers, deepseq, mkDerivation, monad-par-extras, mtl, mwc-random, parallel, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, test-framework-th, time }:
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
          testHaskellDepends = [
            abstract-deque
            abstract-par
            array
            base
            containers
            deepseq
            HUnit
            monad-par-extras
            mtl
            mwc-random
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            test-framework-th
            time
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
      mono-traversable = callPackage ({ HUnit, QuickCheck, base, bytestring, containers, criterion, foldl, hashable, hspec, mkDerivation, mwc-random, semigroups, split, stdenv, text, transformers, unordered-containers, vector, vector-algorithms }:
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
          testHaskellDepends = [
            base
            bytestring
            containers
            foldl
            hspec
            HUnit
            QuickCheck
            semigroups
            text
            transformers
            unordered-containers
            vector
          ];
          benchmarkHaskellDepends = [
            base
            criterion
            mwc-random
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/snoyberg/mono-traversable";
          description = "Type classes for mapping, folding, and traversing monomorphic containers";
          license = stdenv.lib.licenses.mit;
        }) {};
      mtl = callPackage ({ base, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "mtl";
          version = "2.2.1";
          sha256 = "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa";
          revision = "1";
          editedCabalFile = "4b5a800fe9edf168fc7ae48c7a3fc2aab6b418ac15be2f1dad43c0f48a494a3b";
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
      nats = callPackage ({ mkDerivation, stdenv }:
      mkDerivation {
          pname = "nats";
          version = "1.1.1";
          sha256 = "1kfl2yy97nb7q0j17v96rl73xvi3z4db9bk0xychc76dax41n78k";
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/nats/";
          description = "Natural numbers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      natural-transformation = callPackage ({ base, containers, mkDerivation, quickcheck-instances, stdenv, tasty, tasty-quickcheck }:
      mkDerivation {
          pname = "natural-transformation";
          version = "0.4";
          sha256 = "1by8xwjc23l6pa9l4iv7zp82dykpll3vc3hgxk0pgva724n8xhma";
          revision = "1";
          editedCabalFile = "83bedd2c7b0e4f8819753d2075036d99483d33bfdd3ba8889cf61fa05fa89ce9";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            containers
            quickcheck-instances
            tasty
            tasty-quickcheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ku-fpg/natural-transformation";
          description = "A natural transformation package";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      neat-interpolation = callPackage ({ HTF, base, base-prelude, mkDerivation, parsec, stdenv, template-haskell, text }:
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
          testHaskellDepends = [
            base-prelude
            HTF
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nikita-volkov/neat-interpolation";
          description = "A quasiquoter for neat and simple multiline text interpolation";
          license = stdenv.lib.licenses.mit;
        }) {};
      network = callPackage ({ HUnit, base, bytestring, doctest, mkDerivation, stdenv, test-framework, test-framework-hunit, unix }:
      mkDerivation {
          pname = "network";
          version = "2.6.3.1";
          sha256 = "1rl2gl37cf4k0ddsq93q15fwdz1l25nhl4w205krbh7d5dg5y12p";
          libraryHaskellDepends = [
            base
            bytestring
            unix
          ];
          testHaskellDepends = [
            base
            bytestring
            doctest
            HUnit
            test-framework
            test-framework-hunit
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
          version = "0.5.1";
          src = fetchgit {
            url = "https://github.com/serokell/network-transport";
            sha256 = "00p4v8l69mh0219l2qnj5zna10q6ngvhrb7v114rmxml1zsp9nsp";
            rev = "0b8f5a7bec389a4ffa653792cfd203c742a0857b";
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
      network-transport-tcp = callPackage ({ base, bytestring, containers, data-accessor, fetchgit, mkDerivation, network, network-transport, network-transport-tests, stdenv, uuid }:
      mkDerivation {
          pname = "network-transport-tcp";
          version = "0.5.1";
          src = fetchgit {
            url = "https://github.com/serokell/network-transport-tcp";
            sha256 = "1176y7dqyk7gn4576zxy7l03l7h4jsq9xw4bpgbm3qfpax6mfimf";
            rev = "a6c04c35f3a1d786bc5e57fd04cf3e2a043179f3";
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
          testHaskellDepends = [
            base
            network
            network-transport
            network-transport-tests
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-distributed.github.com";
          description = "TCP instantiation of Network.Transport";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      network-uri = callPackage ({ HUnit, base, deepseq, mkDerivation, parsec, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2 }:
      mkDerivation {
          pname = "network-uri";
          version = "2.6.1.0";
          sha256 = "1w27zkvn39kjr9lmw9421y8w43h572ycsfafsb7kyvr3a4ihlgj2";
          revision = "1";
          editedCabalFile = "62cc45c66023e37ef921d5fb546aca56a9c786615e05925fb193a70bf0913690";
          libraryHaskellDepends = [
            base
            deepseq
            parsec
          ];
          testHaskellDepends = [
            base
            HUnit
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/network-uri";
          description = "URI manipulation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      node-sketch = callPackage ({ QuickCheck, aeson, async, attoparsec, base, binary, bytestring, containers, criterion, data-default, deepseq, ekg, ekg-core, ether, exceptions, fetchgit, formatting, hashable, hspec, kademlia, lens, lifted-base, log-warper, mkDerivation, mmorph, monad-control, mtl, mwc-random, network, network-transport, network-transport-inmemory, network-transport-tcp, quickcheck-instances, random, resourcet, semigroups, serokell-util, statistics, stdenv, stm, store, tagged, text, text-format, time, time-units, transformers, transformers-base, transformers-lift, universum, unordered-containers, vector }:
      mkDerivation {
          pname = "node-sketch";
          version = "0.1.2.1";
          src = fetchgit {
            url = "https://github.com/serokell/time-warp-nt.git";
            sha256 = "19khzhvy1a3j1qs2c8ck1hy3swd58b3fpz0w6k1ww97yjd919ksz";
            rev = "8bfba580e628791a03b0bec4d913890dde16811d";
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
            data-default
            deepseq
            ekg
            ekg-core
            ether
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
            store
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
            async
            base
            binary
            bytestring
            containers
            criterion
            mwc-random
            network-transport
            network-transport-tcp
            random
            statistics
            stm
            store
            time
            time-units
            vector
          ];
          testHaskellDepends = [
            base
            binary
            bytestring
            containers
            hspec
            lens
            mtl
            network-transport
            network-transport-inmemory
            network-transport-tcp
            QuickCheck
            quickcheck-instances
            random
            serokell-util
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
          editedCabalFile = "fa998be2c7e00cd26a6e9075bea790caaf3932caa3e9497ad69bc20380dd6911";
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
          editedCabalFile = "c91fbb3ee73d20ccd015842b30f1f29a304893ebe0ae3128b7bbc13d5bb0d4c8";
          libraryHaskellDepends = [
            base
            old-locale
          ];
          doHaddock = false;
          doCheck = false;
          description = "Time library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      optional-args = callPackage ({ base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "optional-args";
          version = "1.0.1";
          sha256 = "16gxy78r868k8hc88hayclhdkn8chyry08fbf99ipyy1xgb081ll";
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Optional function arguments";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      optparse-applicative = callPackage ({ QuickCheck, ansi-wl-pprint, base, mkDerivation, process, stdenv, transformers, transformers-compat }:
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
          testHaskellDepends = [
            base
            QuickCheck
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
      optparse-text = callPackage ({ base, hspec, mkDerivation, optparse-applicative, stdenv, text }:
      mkDerivation {
          pname = "optparse-text";
          version = "0.1.1.0";
          sha256 = "1vw410q7vayfgfbrxpigkpnwm8x1lcdxkh959w9f82l8xkn83h7n";
          libraryHaskellDepends = [
            base
            optparse-applicative
            text
          ];
          testHaskellDepends = [
            base
            hspec
            optparse-applicative
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/passy/optparse-text#readme";
          description = "Data.Text helpers for optparse-applicative";
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
      parsec = callPackage ({ HUnit, base, bytestring, mkDerivation, mtl, stdenv, test-framework, test-framework-hunit, text }:
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
          testHaskellDepends = [
            base
            HUnit
            test-framework
            test-framework-hunit
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/aslatter/parsec";
          description = "Monadic parser combinators";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      parsers = callPackage ({ QuickCheck, attoparsec, base, base-orphans, bytestring, charset, containers, directory, doctest, filepath, mkDerivation, parsec, quickcheck-instances, scientific, stdenv, text, transformers, unordered-containers }:
      mkDerivation {
          pname = "parsers";
          version = "0.12.4";
          sha256 = "07najh7f9y3ahh42z96sw4hnd0kc4x3wm0xlf739y0gh81ys5097";
          revision = "1";
          editedCabalFile = "b1094791062f6d334ccd61466173bee4f906a6a41c30658cec5a96b59a97c3f8";
          libraryHaskellDepends = [
            attoparsec
            base
            base-orphans
            charset
            containers
            parsec
            scientific
            text
            transformers
            unordered-containers
          ];
          testHaskellDepends = [
            attoparsec
            base
            bytestring
            containers
            directory
            doctest
            filepath
            parsec
            QuickCheck
            quickcheck-instances
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/parsers/";
          description = "Parsing combinators";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pem = callPackage ({ HUnit, QuickCheck, base, base64-bytestring, bytestring, mkDerivation, mtl, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2 }:
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
          testHaskellDepends = [
            base
            bytestring
            HUnit
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/vincenthz/hs-pem";
          description = "Privacy Enhanced Mail (PEM) format reader and writer";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      plutus-prototype = callPackage ({ base, bifunctors, binary, bytestring, cardano-crypto, cryptonite, fetchgit, filepath, lens, memory, mkDerivation, mtl, parsec, stdenv, transformers }:
      mkDerivation {
          pname = "plutus-prototype";
          version = "0.1.0.0";
          src = fetchgit {
            url = "https://github.com/input-output-hk/plutus-prototype";
            sha256 = "0h2zq1kcss3f43yqhbz8bjpyxfqlf1wkkdwr91vdkcbjmbgkm8hb";
            rev = "e2e2711e6978002279b4d7c49cab1aff47a2fd43";
          };
          libraryHaskellDepends = [
            base
            bifunctors
            binary
            bytestring
            cardano-crypto
            cryptonite
            filepath
            lens
            memory
            mtl
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
      pretty = callPackage ({ QuickCheck, base, deepseq, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "pretty";
          version = "1.1.3.3";
          sha256 = "164p5ybgf72hfpd3zsn8qpdxipn1pc1nl775jvn0kiqwymwjcqrv";
          libraryHaskellDepends = [
            base
            deepseq
            ghc-prim
          ];
          testHaskellDepends = [
            base
            deepseq
            ghc-prim
            QuickCheck
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
          editedCabalFile = "6ec7c2455c437aba71f856b797e7db440c83719509aa63a9a3d1b4652ca3683d";
          libraryHaskellDepends = [
            base
            ghc-prim
            transformers
          ];
          testHaskellDepends = [
            base
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/primitive";
          description = "Primitive memory-related operations";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      process = callPackage ({ base, bytestring, deepseq, directory, filepath, mkDerivation, stdenv, unix }:
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
          testHaskellDepends = [
            base
            bytestring
            directory
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
          editedCabalFile = "530cbe1328db594389d931c3d5dac1e6e923447d2046901d3065e1098cda1fe0";
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
      psqueues = callPackage ({ HUnit, PSQueue, QuickCheck, array, base, containers, criterion, deepseq, fingertree-psqueue, ghc-prim, hashable, mkDerivation, mtl, random, stdenv, tagged, test-framework, test-framework-hunit, test-framework-quickcheck2, unordered-containers }:
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
          testHaskellDepends = [
            array
            base
            deepseq
            ghc-prim
            hashable
            HUnit
            QuickCheck
            tagged
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
          ];
          benchmarkHaskellDepends = [
            base
            containers
            criterion
            deepseq
            fingertree-psqueue
            ghc-prim
            hashable
            mtl
            PSQueue
            random
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          description = "Pure priority search queues";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pureMD5 = callPackage ({ QuickCheck, base, binary, bytestring, cereal, crypto-api, crypto-api-tests, mkDerivation, pretty-hex, stdenv, tagged, test-framework, test-framework-quickcheck2 }:
      mkDerivation {
          pname = "pureMD5";
          version = "2.1.3";
          sha256 = "0zdilz41cla2ck7mcw1a9702gyg2abq94mqahr4vci9sbs53bwxy";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            cereal
            crypto-api
            tagged
          ];
          testHaskellDepends = [
            base
            binary
            bytestring
            cereal
            crypto-api-tests
            pretty-hex
            QuickCheck
            test-framework
            test-framework-quickcheck2
          ];
          doHaddock = false;
          doCheck = false;
          description = "A Haskell-only implementation of the MD5 digest (hash) algorithm";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      purescript-bridge = callPackage ({ base, containers, directory, filepath, generic-deriving, hspec, hspec-expectations-pretty-diff, lens, mkDerivation, mtl, stdenv, text, transformers }:
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
          testHaskellDepends = [
            base
            containers
            hspec
            hspec-expectations-pretty-diff
            text
          ];
          doHaddock = false;
          doCheck = false;
          description = "Generate PureScript data types from Haskell data types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      pvss = callPackage ({ base, binary, bytestring, cryptonite, cryptonite-openssl, deepseq, hourglass, integer-gmp, memory, mkDerivation, stdenv, tasty, tasty-quickcheck }:
      mkDerivation {
          pname = "pvss";
          version = "0.1";
          sha256 = "16gwq23d7p34n23ydi82lxz7cjvwdc684s36915fb2hm8k60n57s";
          revision = "1";
          editedCabalFile = "2d6b823ed5c0e8852c2d91c248b09cabf83409fb71bd473ab15c44b30427dd0e";
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
          testHaskellDepends = [
            base
            cryptonite
            tasty
            tasty-quickcheck
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
          editedCabalFile = "4321c16dfe0d3c08bba1425d1058261b4b8b553ea5c5c01bd982c9d9e23b39ec";
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
          editedCabalFile = "7b67624fd76ddf97c206de0801dc7e888097e9d572974be9b9ea6551d76965df";
          libraryHaskellDepends = [
            base
            time
          ];
          testHaskellDepends = [ base ];
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
      recursion-schemes = callPackage ({ HUnit, base, base-orphans, bifunctors, comonad, free, mkDerivation, semigroups, stdenv, template-haskell, transformers, transformers-compat }:
      mkDerivation {
          pname = "recursion-schemes";
          version = "5.0.1";
          sha256 = "00zz8gxf3ha1zq6m2hzcia35cry5wgvs29h60s2jvp3yzmr7radp";
          revision = "1";
          editedCabalFile = "36143fa4a8c0474a6799fc6975a051cecfdafb72a34d43a10cd53e395286ae38";
          libraryHaskellDepends = [
            base
            base-orphans
            bifunctors
            comonad
            free
            semigroups
            template-haskell
            transformers
            transformers-compat
          ];
          testHaskellDepends = [
            base
            HUnit
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ekmett/recursion-schemes/";
          description = "Generalized bananas, lenses and barbed wire";
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
      regex-compat = callPackage ({ array, base, mkDerivation, regex-base, regex-posix, stdenv }:
      mkDerivation {
          pname = "regex-compat";
          version = "0.95.1";
          sha256 = "0fwmima3f04p9y4h3c23493n1xj629ia2dxaisqm6rynljjv2z6m";
          libraryHaskellDepends = [
            array
            base
            regex-base
            regex-posix
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
      resourcet = callPackage ({ base, containers, exceptions, hspec, lifted-base, mkDerivation, mmorph, monad-control, mtl, stdenv, transformers, transformers-base, transformers-compat }:
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
          testHaskellDepends = [
            base
            hspec
            lifted-base
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/snoyberg/conduit";
          description = "Deterministic allocation and freeing of scarce resources";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      rocksdb-haskell = callPackage ({ QuickCheck, base, binary, bytestring, data-default, filepath, hspec, hspec-expectations, mkDerivation, process, resourcet, rocksdb, stdenv, temporary, transformers }:
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
          testHaskellDepends = [
            base
            bytestring
            data-default
            hspec
            hspec-expectations
            process
            QuickCheck
            resourcet
            temporary
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/serokell/rocksdb-haskell";
          description = "Haskell bindings to RocksDB";
          license = stdenv.lib.licenses.bsd3;
        }) { rocksdb = pkgs.rocksdb; };
      safe = callPackage ({ QuickCheck, base, deepseq, mkDerivation, stdenv }:
      mkDerivation {
          pname = "safe";
          version = "0.3.14";
          sha256 = "13y8zlvifwwr5ybizqw0d1lzr763fnzlqsm8m5a1whpn933hqn6v";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            deepseq
            QuickCheck
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/ndmitchell/safe#readme";
          description = "Library of safe (exception free) functions";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      safecopy = callPackage ({ QuickCheck, array, base, bytestring, cereal, containers, lens, lens-action, mkDerivation, old-time, quickcheck-instances, semigroups, stdenv, tasty, tasty-quickcheck, template-haskell, text, time, vector }:
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
          testHaskellDepends = [
            array
            base
            cereal
            containers
            lens
            lens-action
            QuickCheck
            quickcheck-instances
            tasty
            tasty-quickcheck
            template-haskell
            time
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/acid-state/safecopy";
          description = "Binary serialization with version control";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      scientific = callPackage ({ QuickCheck, base, binary, bytestring, containers, criterion, deepseq, ghc-prim, hashable, integer-gmp, integer-logarithms, mkDerivation, smallcheck, stdenv, tasty, tasty-ant-xml, tasty-hunit, tasty-quickcheck, tasty-smallcheck, text, vector }:
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
          testHaskellDepends = [
            base
            binary
            bytestring
            QuickCheck
            smallcheck
            tasty
            tasty-ant-xml
            tasty-hunit
            tasty-quickcheck
            tasty-smallcheck
            text
          ];
          benchmarkHaskellDepends = [
            base
            criterion
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/basvandijk/scientific";
          description = "Numbers represented using scientific notation";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      scotty = callPackage ({ aeson, async, base, blaze-builder, bytestring, case-insensitive, data-default-class, directory, fail, hspec, hspec-wai, http-types, lifted-base, mkDerivation, monad-control, mtl, nats, network, regex-compat, stdenv, text, transformers, transformers-base, transformers-compat, wai, wai-extra, warp }:
      mkDerivation {
          pname = "scotty";
          version = "0.11.0";
          sha256 = "1vc6lc8q1cqqq67y78c70sw2jim8ps7bgp85a2gjgwfc6z4h68l9";
          revision = "5";
          editedCabalFile = "ae76edc7f78a68ecf982aaa2d4421e80796a365fbb13c38b1cf0f77c3586e482";
          libraryHaskellDepends = [
            aeson
            base
            blaze-builder
            bytestring
            case-insensitive
            data-default-class
            fail
            http-types
            monad-control
            mtl
            nats
            network
            regex-compat
            text
            transformers
            transformers-base
            transformers-compat
            wai
            wai-extra
            warp
          ];
          testHaskellDepends = [
            async
            base
            data-default-class
            directory
            hspec
            hspec-wai
            http-types
            lifted-base
            network
            text
            wai
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/scotty-web/scotty";
          description = "Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      semigroupoids = callPackage ({ base, base-orphans, bifunctors, comonad, containers, contravariant, directory, distributive, doctest, filepath, mkDerivation, semigroups, stdenv, tagged, transformers, transformers-compat }:
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
          testHaskellDepends = [
            base
            directory
            doctest
            filepath
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
      serokell-util = callPackage ({ QuickCheck, acid-state, aeson, aeson-extra, ansi-terminal, base, base16-bytestring, base64-bytestring, binary, binary-orphans, bytestring, cereal, cereal-vector, clock, containers, data-msgpack, deepseq, directory, either, exceptions, extra, filepath, formatting, hashable, hspec, lens, log-warper, mkDerivation, monad-control, mtl, optparse-applicative, parsec, quickcheck-instances, safecopy, scientific, semigroups, stdenv, stm, template-haskell, text, text-format, time-units, transformers, universum, unordered-containers, vector, yaml }:
      mkDerivation {
          pname = "serokell-util";
          version = "0.1.5.0";
          sha256 = "0jn73f1jx57sj69a4xnj5nn1hj3m3mdakwjvdkx4a2k5qvwcvny8";
          libraryHaskellDepends = [
            acid-state
            aeson
            aeson-extra
            ansi-terminal
            base
            base16-bytestring
            base64-bytestring
            binary
            binary-orphans
            bytestring
            cereal
            cereal-vector
            clock
            containers
            data-msgpack
            deepseq
            directory
            either
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
          testHaskellDepends = [
            aeson
            base
            binary
            bytestring
            cereal
            data-msgpack
            hspec
            QuickCheck
            quickcheck-instances
            safecopy
            scientific
            text
            text-format
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/serokell/serokell-util";
          description = "General-purpose functions by Serokell";
          license = stdenv.lib.licenses.mit;
        }) {};
      servant-blaze = callPackage ({ base, blaze-html, http-media, mkDerivation, servant_0_10, stdenv }:
      mkDerivation {
          pname = "servant-blaze";
          version = "0.7.1";
          sha256 = "0ii60xn5khsj8w3glvwqpwrpd6v9yc1n52gk9qsfwfxq49x1rvch";
          revision = "5";
          editedCabalFile = "91ea95dd30a2600c20a1304dad3d6e6a87fac4718438aa2e1822aa6af704ff17";
          libraryHaskellDepends = [
            base
            blaze-html
            http-media
            servant_0_10
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-servant.readthedocs.org/";
          description = "Blaze-html support for servant";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      servant-multipart = callPackage ({ base, bytestring, directory, http-client, http-media, mkDerivation, network, resourcet, servant-server_0_10, servant_0_10, stdenv, text, transformers, wai, wai-extra, warp }:
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
            servant_0_10
            servant-server_0_10
            text
            transformers
            wai
            wai-extra
          ];
          executableHaskellDepends = [
            base
            http-client
            network
            servant_0_10
            servant-server_0_10
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
      servant-server_0_10 = callPackage ({ Cabal, QuickCheck, aeson, attoparsec, base, base-compat, base64-bytestring, bytestring, containers, directory, doctest, exceptions, filemanip, filepath, hspec, hspec-wai, http-api-data, http-types, mkDerivation, monad-control, mtl, network, network-uri, parsec, resourcet, safe, servant_0_10, should-not-typecheck, split, stdenv, string-conversions, system-filepath, temporary, text, transformers, transformers-base, transformers-compat, wai, wai-app-static, wai-extra, warp, word8 }:
      mkDerivation {
          pname = "servant-server";
          version = "0.10";
          sha256 = "0g87g48p179v1j3ki3vsvkk5gidqfp5yb9xwnh0j90v7x8ilvlcr";
          revision = "1";
          editedCabalFile = "4332581ae3248c16017f88461abf6eef9fbad2ca86e86a2e8a013f9adc665973";
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
            servant_0_10
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
            servant_0_10
            text
            wai
            warp
          ];
          testHaskellDepends = [
            aeson
            base
            base-compat
            base64-bytestring
            bytestring
            directory
            doctest
            exceptions
            filemanip
            filepath
            hspec
            hspec-wai
            http-types
            mtl
            network
            parsec
            QuickCheck
            resourcet
            safe
            servant_0_10
            should-not-typecheck
            string-conversions
            temporary
            text
            transformers
            transformers-compat
            wai
            wai-extra
            warp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-servant.readthedocs.org/";
          description = "A family of combinators for defining webservices APIs and serving them";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      servant-swagger-ui = callPackage ({ aeson, base, base-compat, blaze-markup, bytestring, directory, file-embed, filepath, http-media, lens, mkDerivation, servant-blaze, servant-server_0_10, servant-swagger_1_1_2_1, servant_0_10, stdenv, swagger2, template-haskell, text, transformers, transformers-compat, wai, wai-app-static, warp }:
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
            servant_0_10
            servant-blaze
            servant-server_0_10
            servant-swagger_1_1_2_1
            swagger2
            template-haskell
            text
            transformers
            transformers-compat
            wai-app-static
          ];
          testHaskellDepends = [
            aeson
            base
            base-compat
            lens
            servant_0_10
            servant-server_0_10
            servant-swagger_1_1_2_1
            swagger2
            text
            transformers
            transformers-compat
            wai
            warp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/servant-swagger-ui#readme";
          description = "Servant swagger ui";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      servant-swagger_1_1_2_1 = callPackage ({ Cabal, QuickCheck, aeson, aeson-qq, base, bytestring, cabal-doctest, directory, doctest, filepath, hspec, http-media, insert-ordered-containers, lens, mkDerivation, servant_0_10, stdenv, swagger2, text, time, unordered-containers }:
      mkDerivation {
          pname = "servant-swagger";
          version = "1.1.2.1";
          sha256 = "0qgrc01y9d2wsfg4r1iq71m2075qg75656wlljqb7pbkywxb0aih";
          revision = "2";
          editedCabalFile = "ca219bf1d1da3541d9072ec0c0a751c9ba7a2a3e1f8f21e0419570088a33e5c1";
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
            servant_0_10
            swagger2
            text
            unordered-containers
          ];
          testHaskellDepends = [
            aeson
            aeson-qq
            base
            directory
            doctest
            filepath
            hspec
            lens
            QuickCheck
            servant_0_10
            swagger2
            text
            time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell-servant/servant-swagger";
          description = "Generate Swagger specification for your servant API";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      servant_0_10 = callPackage ({ Cabal, QuickCheck, aeson, aeson-compat, attoparsec, base, base-compat, bytestring, case-insensitive, directory, doctest, filemanip, filepath, hspec, http-api-data, http-media, http-types, mkDerivation, mmorph, mtl, natural-transformation, network-uri, quickcheck-instances, stdenv, string-conversions, text, url, vault }:
      mkDerivation {
          pname = "servant";
          version = "0.10";
          sha256 = "07ik9ddaj1vmq37dl4mg00rawa9phfapm8a52cs1b5km5fxaknp1";
          revision = "2";
          editedCabalFile = "6808bd35e2105f94f1290066a773cd302eb0c01c987e6e933de1fc5fb05f398f";
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
          testHaskellDepends = [
            aeson
            aeson-compat
            attoparsec
            base
            base-compat
            bytestring
            directory
            doctest
            filemanip
            filepath
            hspec
            QuickCheck
            quickcheck-instances
            string-conversions
            text
            url
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-servant.readthedocs.org/";
          description = "A family of combinators for defining webservices APIs";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      setenv = callPackage ({ base, mkDerivation, stdenv, unix }:
      mkDerivation {
          pname = "setenv";
          version = "0.1.1.3";
          sha256 = "0cnbgrvb9byyahb37zlqrj05rj25v190crgcw8wmlgf0mwwxyn73";
          revision = "1";
          editedCabalFile = "c5916ac0d2a828473cd171261328a290afe0abd799db1ac8c310682fe778c45b";
          libraryHaskellDepends = [
            base
            unix
          ];
          doHaddock = false;
          doCheck = false;
          description = "A cross-platform library for setting environment variables";
          license = stdenv.lib.licenses.mit;
        }) {};
      simple-sendfile = callPackage ({ HUnit, base, bytestring, conduit, conduit-extra, directory, hspec, mkDerivation, network, process, resourcet, stdenv, unix }:
      mkDerivation {
          pname = "simple-sendfile";
          version = "0.2.25";
          sha256 = "0k99j9xfcf83c55jmn202hdinhjaa4yn3dal4rvjk2w2rlhqirha";
          revision = "1";
          editedCabalFile = "ac78b431148355d859f1b432ce367faf04ba14c244b30818fd0ffc28ec86afab";
          libraryHaskellDepends = [
            base
            bytestring
            network
            unix
          ];
          testHaskellDepends = [
            base
            bytestring
            conduit
            conduit-extra
            directory
            hspec
            HUnit
            network
            process
            resourcet
            unix
          ];
          doHaddock = false;
          doCheck = false;
          description = "Cross platform library for the sendfile system call";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      smallcheck = callPackage ({ base, ghc-prim, logict, mkDerivation, mtl, pretty, stdenv }:
      mkDerivation {
          pname = "smallcheck";
          version = "1.1.1";
          sha256 = "1ygrabxh40bym3grnzqyfqn96lirnxspb8cmwkkr213239y605sd";
          revision = "1";
          editedCabalFile = "b19c841b12cc34f6379c2b72bc4c250da9b0346c46690dae419caaa0310478fa";
          libraryHaskellDepends = [
            base
            ghc-prim
            logict
            mtl
            pretty
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/feuerbach/smallcheck";
          description = "A property-based testing library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      snap-core = callPackage ({ HUnit, QuickCheck, attoparsec, base, bytestring, bytestring-builder, case-insensitive, containers, deepseq, directory, filepath, hashable, io-streams, lifted-base, mkDerivation, monad-control, mtl, network, network-uri, old-locale, parallel, random, readable, regex-posix, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text, time, transformers, transformers-base, unix-compat, unordered-containers, vector, zlib }:
      mkDerivation {
          pname = "snap-core";
          version = "1.0.2.0";
          sha256 = "0p5nwm8b85h1b4q8lr955vhyw7l8xcfpkqbdkicrr6qn4x3vm3p4";
          revision = "1";
          editedCabalFile = "fd40a0961a8517e3566670b095595ac2ac20ee4ea2615b32479046ac498a6b05";
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
          testHaskellDepends = [
            attoparsec
            base
            bytestring
            bytestring-builder
            case-insensitive
            containers
            deepseq
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
            parallel
            QuickCheck
            random
            readable
            regex-posix
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
            time
            transformers
            transformers-base
            unix-compat
            unordered-containers
            vector
            zlib
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://snapframework.com/";
          description = "Snap: A Haskell Web Framework (core interfaces and types)";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      snap-server = callPackage ({ HUnit, QuickCheck, attoparsec, base, base16-bytestring, blaze-builder, bytestring, bytestring-builder, case-insensitive, clock, containers, criterion, deepseq, directory, filepath, http-common, http-streams, io-streams, io-streams-haproxy, lifted-base, mkDerivation, monad-control, mtl, network, old-locale, parallel, random, snap-core, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text, threads, time, transformers, unix, unix-compat, vector }:
      mkDerivation {
          pname = "snap-server";
          version = "1.0.2.0";
          sha256 = "0jfmbc6dx4jl1jvx89v5ghw3q298wbdkzgmvnn1fxbiiadcjjzv7";
          revision = "1";
          editedCabalFile = "bc24161a1b1eec7d7568f34dcc4b3d44a9b6df793c5e63aa5f03ca8681984553";
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
          testHaskellDepends = [
            attoparsec
            base
            base16-bytestring
            blaze-builder
            bytestring
            bytestring-builder
            case-insensitive
            clock
            containers
            deepseq
            directory
            filepath
            http-common
            http-streams
            HUnit
            io-streams
            io-streams-haproxy
            lifted-base
            monad-control
            mtl
            network
            old-locale
            parallel
            QuickCheck
            random
            snap-core
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
            threads
            time
            transformers
            unix
            unix-compat
            vector
          ];
          benchmarkHaskellDepends = [
            attoparsec
            base
            blaze-builder
            bytestring
            bytestring-builder
            criterion
            io-streams
            io-streams-haproxy
            snap-core
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://snapframework.com/";
          description = "A web server for the Snap Framework";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      socks = callPackage ({ base, bytestring, cereal, mkDerivation, network, stdenv }:
      mkDerivation {
          pname = "socks";
          version = "0.5.5";
          sha256 = "0s689w1hh9g8ifl75xhzbv96ir07hwn04b4lgvbxzl8swa9ylir6";
          revision = "1";
          editedCabalFile = "7cd766c60366c9b1e6100af4f710fd38bc851ac28387b60471c6a3b63bc0e85b";
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
      split = callPackage ({ QuickCheck, base, mkDerivation, stdenv }:
      mkDerivation {
          pname = "split";
          version = "0.2.3.1";
          sha256 = "12660m16c6sib3laaxn6w9s453pyx1b49myrmzfij372vq5bc5bn";
          revision = "1";
          editedCabalFile = "6089e920a72947806dff273664af651f5f128339fab9fc1d823bfedb102a6ecd";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            QuickCheck
          ];
          doHaddock = false;
          doCheck = false;
          description = "Combinator library for splitting lists";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      statistics = callPackage ({ HUnit, QuickCheck, aeson, base, binary, deepseq, erf, ieee754, math-functions, mkDerivation, monad-par, mwc-random, primitive, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, vector, vector-algorithms, vector-binary-instances }:
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
          testHaskellDepends = [
            base
            binary
            erf
            HUnit
            ieee754
            math-functions
            mwc-random
            primitive
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            vector
            vector-algorithms
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
          editedCabalFile = "49cfd80cba95f84d42eda0045346c8a567df5ce434d4da3d26ac3e977826fc4f";
          libraryHaskellDepends = [
            array
            base
          ];
          doHaddock = false;
          doCheck = false;
          description = "Software Transactional Memory";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      stm-containers = callPackage ({ HTF, QuickCheck, async, base, base-prelude, containers, criterion, focus, free, hashable, hashtables, list-t, loch-th, mkDerivation, mtl, mtl-prelude, mwc-random, mwc-random-monad, placeholders, primitive, stdenv, text, transformers, unordered-containers }:
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
          testHaskellDepends = [
            base
            base-prelude
            focus
            free
            hashable
            HTF
            list-t
            loch-th
            mtl
            mtl-prelude
            placeholders
            primitive
            QuickCheck
            transformers
            unordered-containers
          ];
          benchmarkHaskellDepends = [
            async
            base
            base-prelude
            containers
            criterion
            focus
            free
            hashable
            hashtables
            list-t
            loch-th
            mtl
            mtl-prelude
            mwc-random
            mwc-random-monad
            placeholders
            text
            unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/nikita-volkov/stm-containers";
          description = "Containers for STM";
          license = stdenv.lib.licenses.mit;
        }) {};
      store = callPackage ({ array, async, base, base-orphans, base64-bytestring, bytestring, cereal, cereal-vector, conduit, containers, contravariant, criterion, cryptohash, deepseq, directory, filepath, free, ghc-prim, hashable, hspec, hspec-smallcheck, integer-gmp, lifted-base, mkDerivation, monad-control, mono-traversable, network, primitive, resourcet, safe, semigroups, smallcheck, stdenv, store-core, streaming-commons, syb, template-haskell, text, th-lift, th-lift-instances, th-orphans, th-reify-many, th-utilities, time, transformers, unordered-containers, vector, vector-binary-instances, void, weigh }:
      mkDerivation {
          pname = "store";
          version = "0.4.3.1";
          sha256 = "0h8b91dpp4r1bhdq3k4zcnaby1ka9x6gy8ypbhv47b2mwpnwp09p";
          libraryHaskellDepends = [
            array
            async
            base
            base-orphans
            base64-bytestring
            bytestring
            conduit
            containers
            contravariant
            cryptohash
            deepseq
            directory
            filepath
            free
            ghc-prim
            hashable
            hspec
            hspec-smallcheck
            integer-gmp
            lifted-base
            monad-control
            mono-traversable
            network
            primitive
            resourcet
            safe
            semigroups
            smallcheck
            store-core
            streaming-commons
            syb
            template-haskell
            text
            th-lift
            th-lift-instances
            th-orphans
            th-reify-many
            th-utilities
            time
            transformers
            unordered-containers
            vector
            void
          ];
          testHaskellDepends = [
            array
            async
            base
            base-orphans
            base64-bytestring
            bytestring
            cereal
            cereal-vector
            conduit
            containers
            contravariant
            criterion
            cryptohash
            deepseq
            directory
            filepath
            free
            ghc-prim
            hashable
            hspec
            hspec-smallcheck
            integer-gmp
            lifted-base
            monad-control
            mono-traversable
            network
            primitive
            resourcet
            safe
            semigroups
            smallcheck
            store-core
            streaming-commons
            syb
            template-haskell
            text
            th-lift
            th-lift-instances
            th-orphans
            th-reify-many
            th-utilities
            time
            transformers
            unordered-containers
            vector
            vector-binary-instances
            void
            weigh
          ];
          benchmarkHaskellDepends = [
            array
            async
            base
            base-orphans
            base64-bytestring
            bytestring
            conduit
            containers
            contravariant
            criterion
            cryptohash
            deepseq
            directory
            filepath
            free
            ghc-prim
            hashable
            hspec
            hspec-smallcheck
            integer-gmp
            lifted-base
            monad-control
            mono-traversable
            network
            primitive
            resourcet
            safe
            semigroups
            smallcheck
            store-core
            streaming-commons
            syb
            template-haskell
            text
            th-lift
            th-lift-instances
            th-orphans
            th-reify-many
            th-utilities
            time
            transformers
            unordered-containers
            vector
            void
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fpco/store#readme";
          description = "Fast binary serialization";
          license = stdenv.lib.licenses.mit;
        }) {};
      store-core = callPackage ({ base, bytestring, fail, ghc-prim, mkDerivation, primitive, stdenv, text, transformers }:
      mkDerivation {
          pname = "store-core";
          version = "0.4";
          sha256 = "1xglxj44lhld222gc7n7klvgkvmxdgynqymi02w45fjskhxzk3da";
          libraryHaskellDepends = [
            base
            bytestring
            fail
            ghc-prim
            primitive
            text
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fpco/store#readme";
          description = "Fast and lightweight binary serialization";
          license = stdenv.lib.licenses.mit;
        }) {};
      streaming-commons = callPackage ({ QuickCheck, array, async, base, blaze-builder, bytestring, criterion, deepseq, directory, hspec, mkDerivation, network, process, random, stdenv, stm, text, transformers, unix, zlib }:
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
          testHaskellDepends = [
            array
            async
            base
            blaze-builder
            bytestring
            deepseq
            hspec
            network
            QuickCheck
            text
            unix
            zlib
          ];
          benchmarkHaskellDepends = [
            base
            blaze-builder
            bytestring
            criterion
            deepseq
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fpco/streaming-commons";
          description = "Common lower-level functions needed by various streaming data libraries";
          license = stdenv.lib.licenses.mit;
        }) {};
      string-conversions = callPackage ({ QuickCheck, base, bytestring, deepseq, hspec, mkDerivation, quickcheck-instances, stdenv, text, utf8-string }:
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
          testHaskellDepends = [
            base
            bytestring
            deepseq
            hspec
            QuickCheck
            quickcheck-instances
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
      swagger2 = callPackage ({ Glob, HUnit, QuickCheck, aeson, aeson-qq, base, base-compat, bytestring, containers, doctest, fetchgit, generics-sop, hashable, hspec, http-media, insert-ordered-containers, lens, mkDerivation, mtl, network, scientific, stdenv, template-haskell, text, time, transformers, unordered-containers, uuid-types, vector }:
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
          testHaskellDepends = [
            aeson
            aeson-qq
            base
            base-compat
            bytestring
            containers
            doctest
            Glob
            hashable
            hspec
            HUnit
            insert-ordered-containers
            lens
            mtl
            QuickCheck
            text
            time
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/GetShopTV/swagger2";
          description = "Swagger 2.0 data model";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      syb = callPackage ({ HUnit, base, containers, mkDerivation, mtl, stdenv }:
      mkDerivation {
          pname = "syb";
          version = "0.6";
          sha256 = "1p3cnqjm13677r4a966zffzhi9b3a321aln8zs8ckqj0d9z1z3d3";
          revision = "1";
          editedCabalFile = "9d5ac26aa923516a2e3705275dff1fa7bff989ff4b607668acc1264c6d7b1695";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            containers
            HUnit
            mtl
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.cs.uu.nl/wiki/GenericProgramming/SYB";
          description = "Scrap Your Boilerplate";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      system-fileio = callPackage ({ base, bytestring, chell, mkDerivation, stdenv, system-filepath, temporary, text, time, transformers, unix }:
      mkDerivation {
          pname = "system-fileio";
          version = "0.3.16.3";
          sha256 = "1484hcl27s2qcby8ws5djj11q9bz68bspcifz9h5gii2ndy70x9i";
          libraryHaskellDepends = [
            base
            bytestring
            system-filepath
            text
            time
            unix
          ];
          testHaskellDepends = [
            base
            bytestring
            chell
            system-filepath
            temporary
            text
            time
            transformers
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fpco/haskell-filesystem";
          description = "Consistent filesystem interaction across GHC versions (deprecated)";
          license = stdenv.lib.licenses.mit;
        }) {};
      system-filepath = callPackage ({ QuickCheck, base, bytestring, chell, chell-quickcheck, deepseq, mkDerivation, stdenv, text }:
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
          testHaskellDepends = [
            base
            bytestring
            chell
            chell-quickcheck
            QuickCheck
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
          editedCabalFile = "a8d7b211a0831f5acf65a36003aebab7673ffb6a874a49715e05e7b76a6cb896";
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
      tar = callPackage ({ QuickCheck, array, base, bytestring, bytestring-handle, containers, criterion, deepseq, directory, filepath, mkDerivation, old-time, stdenv, tasty, tasty-quickcheck, time }:
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
          testHaskellDepends = [
            array
            base
            bytestring
            bytestring-handle
            containers
            deepseq
            directory
            filepath
            QuickCheck
            tasty
            tasty-quickcheck
            time
          ];
          benchmarkHaskellDepends = [
            array
            base
            bytestring
            containers
            criterion
            deepseq
            directory
            filepath
            old-time
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
      text = callPackage ({ HUnit, QuickCheck, array, base, binary, bytestring, deepseq, directory, ghc-prim, integer-gmp, mkDerivation, quickcheck-unicode, random, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2 }:
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
          testHaskellDepends = [
            array
            base
            binary
            bytestring
            deepseq
            directory
            ghc-prim
            HUnit
            integer-gmp
            QuickCheck
            quickcheck-unicode
            random
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/text";
          description = "An efficient packed Unicode text type";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      text-binary = callPackage ({ base, binary, mkDerivation, stdenv, text }:
      mkDerivation {
          pname = "text-binary";
          version = "0.2.1.1";
          sha256 = "18gl10pwg3qwsk0za3c70j4n6a9129wwf1b7d3a461h816yv55xn";
          libraryHaskellDepends = [
            base
            binary
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/kawu/text-binary";
          description = "Binary instances for text types";
          license = stdenv.lib.licenses.bsd2;
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
          testHaskellDepends = [
            base
            containers
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
          testHaskellDepends = [
            base
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
          testHaskellDepends = [
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
      th-lift-instances = callPackage ({ QuickCheck, base, bytestring, containers, mkDerivation, stdenv, template-haskell, text, th-lift, vector }:
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
          testHaskellDepends = [
            base
            bytestring
            containers
            QuickCheck
            template-haskell
            text
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/bennofs/th-lift-instances/";
          description = "Lift instances for template-haskell for common data types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      th-orphans = callPackage ({ base, hspec, mkDerivation, mtl, stdenv, template-haskell, th-lift, th-lift-instances, th-reify-many }:
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
          testHaskellDepends = [
            base
            hspec
            template-haskell
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
          testHaskellDepends = [
            base
            template-haskell
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/mgsloan/th-reify-many";
          description = "Recurseively reify template haskell datatype info";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      th-utilities = callPackage ({ base, bytestring, containers, directory, filepath, hspec, mkDerivation, primitive, stdenv, syb, template-haskell, text, th-orphans, vector }:
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
          testHaskellDepends = [
            base
            bytestring
            containers
            directory
            filepath
            hspec
            primitive
            syb
            template-haskell
            text
            th-orphans
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fpco/th-utilities#readme";
          description = "Collection of useful functions for use with Template Haskell";
          license = stdenv.lib.licenses.mit;
        }) {};
      time = callPackage ({ QuickCheck, base, deepseq, mkDerivation, stdenv, test-framework, test-framework-quickcheck2, unix }:
      mkDerivation {
          pname = "time";
          version = "1.6.0.1";
          sha256 = "1jvzgifkalfypbm479fzxb7yi8d5z00b4y6hf6qjdlpl71pv8sgz";
          libraryHaskellDepends = [
            base
            deepseq
          ];
          testHaskellDepends = [
            base
            deepseq
            QuickCheck
            test-framework
            test-framework-quickcheck2
            unix
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
      time-parsers = callPackage ({ attoparsec, base, bifunctors, mkDerivation, parsec, parsers, stdenv, tasty, tasty-hunit, template-haskell, text, time }:
      mkDerivation {
          pname = "time-parsers";
          version = "0.1.2.0";
          sha256 = "091wpcqj1kjvyjgj1y1invn0g5lhdxc92az2bcbwbrpq2c7x8l2f";
          revision = "1";
          editedCabalFile = "f2a522da59c7dab618b37126dcd5e183658e0d46e13c7a56243b10b1541873bb";
          libraryHaskellDepends = [
            base
            parsers
            template-haskell
            time
          ];
          testHaskellDepends = [
            attoparsec
            base
            bifunctors
            parsec
            parsers
            tasty
            tasty-hunit
            template-haskell
            text
            time
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/phadej/time-parsers#readme";
          description = "Parsers for types in `time`";
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
      tls = callPackage ({ QuickCheck, asn1-encoding, asn1-types, async, base, bytestring, cereal, criterion, cryptonite, data-default-class, hourglass, memory, mkDerivation, mtl, network, stdenv, tasty, tasty-quickcheck, transformers, x509, x509-store, x509-validation }:
      mkDerivation {
          pname = "tls";
          version = "1.3.10";
          sha256 = "0w81bv1khibp36mnqb2685y01s3v1032l3qf1y6wx9fx807ps1cz";
          revision = "2";
          editedCabalFile = "30f94541fc229715b10e6752cc25671fba874a7564de8ff64df0ce64f427e39c";
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
          testHaskellDepends = [
            base
            bytestring
            cereal
            cryptonite
            data-default-class
            hourglass
            mtl
            QuickCheck
            tasty
            tasty-quickcheck
            x509
            x509-validation
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
            cryptonite
            data-default-class
            hourglass
            mtl
            QuickCheck
            tasty-quickcheck
            x509
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
          editedCabalFile = "fb1a305f29cbf6ac182af7e67efaae9fcb9664d8d9606bb8a7f3414ad4c8d7a4";
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
      turtle = callPackage ({ ansi-wl-pprint, async, base, bytestring, clock, criterion, directory, doctest, foldl, hostname, managed, mkDerivation, optional-args, optparse-applicative, process, semigroups, stdenv, stm, system-fileio, system-filepath, temporary, text, time, transformers, unix, unix-compat }:
      mkDerivation {
          pname = "turtle";
          version = "1.3.3";
          sha256 = "07jd62b0m1a5g32rl3lgqcwhj8zk3s4gcnqy0c7yiqww7z8nz8c2";
          libraryHaskellDepends = [
            ansi-wl-pprint
            async
            base
            bytestring
            clock
            directory
            foldl
            hostname
            managed
            optional-args
            optparse-applicative
            process
            semigroups
            stm
            system-fileio
            system-filepath
            temporary
            text
            time
            transformers
            unix
            unix-compat
          ];
          testHaskellDepends = [
            base
            doctest
          ];
          benchmarkHaskellDepends = [
            base
            criterion
            text
          ];
          doHaddock = false;
          doCheck = false;
          description = "Shell programming, Haskell-style";
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
          editedCabalFile = "98424c728917bd4638112a913f0032be8b84e837f4f60fc96e0d6dc40d61e5c6";
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
          editedCabalFile = "3db1b6e8de36a36fc4f979e1045e82554f16c736961fa0392e42b7b3f4decfd4";
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
          revision = "1";
          editedCabalFile = "6c1914a5322b96837ac47296bf0ce287ce9c89cc131f844483f5d9784e36910a";
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
      unix-time = callPackage ({ QuickCheck, base, binary, bytestring, doctest, hspec, mkDerivation, old-locale, old-time, stdenv, time }:
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
          testHaskellDepends = [
            base
            bytestring
            doctest
            hspec
            old-locale
            old-time
            QuickCheck
            time
          ];
          doHaddock = false;
          doCheck = false;
          description = "Unix time parser/formatter and utilities";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      unordered-containers = callPackage ({ ChasingBottoms, HUnit, QuickCheck, base, bytestring, containers, criterion, deepseq, deepseq-generics, hashable, hashmap, mkDerivation, mtl, random, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2 }:
      mkDerivation {
          pname = "unordered-containers";
          version = "0.2.8.0";
          sha256 = "1a7flszhhgyjn0nm9w7cm26jbf6vyx9ij1iij4sl11pjkwsqi8d4";
          libraryHaskellDepends = [
            base
            deepseq
            hashable
          ];
          testHaskellDepends = [
            base
            ChasingBottoms
            containers
            hashable
            HUnit
            QuickCheck
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            containers
            criterion
            deepseq
            deepseq-generics
            hashable
            hashmap
            mtl
            random
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/tibbe/unordered-containers";
          description = "Efficient hashing-based container types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      uri-bytestring = callPackage ({ HUnit, QuickCheck, attoparsec, base, blaze-builder, bytestring, containers, criterion, deepseq, deepseq-generics, generics-sop, lens-simple, mkDerivation, network-uri, quickcheck-instances, semigroups, stdenv, tasty, tasty-hunit, tasty-quickcheck, template-haskell, th-lift-instances }:
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
          testHaskellDepends = [
            attoparsec
            base
            blaze-builder
            bytestring
            containers
            generics-sop
            HUnit
            lens-simple
            QuickCheck
            quickcheck-instances
            semigroups
            tasty
            tasty-hunit
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [
            base
            blaze-builder
            bytestring
            criterion
            deepseq
            deepseq-generics
            network-uri
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
          editedCabalFile = "19d60820611ed14041c63bd240958a652276b68d4ca3cf6042864a166fd227ad";
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
      uuid = callPackage ({ HUnit, QuickCheck, base, binary, bytestring, criterion, cryptohash-md5, cryptohash-sha1, entropy, mersenne-random-pure64, mkDerivation, network-info, random, stdenv, tasty, tasty-hunit, tasty-quickcheck, text, time, uuid-types }:
      mkDerivation {
          pname = "uuid";
          version = "1.3.13";
          sha256 = "09xhk42yhxvqmka0iqrv3338asncz8cap3j0ic0ps896f2581b6z";
          revision = "1";
          editedCabalFile = "75651c8f3fdbac591f5c9057093ff4de3a4a31ff73625c754ced00ae3f0ce07a";
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
          testHaskellDepends = [
            base
            bytestring
            HUnit
            QuickCheck
            random
            tasty
            tasty-hunit
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [
            base
            criterion
            mersenne-random-pure64
            random
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/hvr/uuid";
          description = "For creating, comparing, parsing and printing Universally Unique Identifiers";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      uuid-types = callPackage ({ HUnit, QuickCheck, base, binary, bytestring, containers, criterion, deepseq, hashable, mkDerivation, random, stdenv, tasty, tasty-hunit, tasty-quickcheck, text }:
      mkDerivation {
          pname = "uuid-types";
          version = "1.0.3";
          sha256 = "1zdka5jnm1h6k36w3nr647yf3b5lqb336g3fkprhd6san9x52xlj";
          revision = "1";
          editedCabalFile = "01887ed945e74c3c361b00700bd9aeead37d1124d39c0d4f190f89fb0e909c47";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            deepseq
            hashable
            random
            text
          ];
          testHaskellDepends = [
            base
            bytestring
            HUnit
            QuickCheck
            tasty
            tasty-hunit
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            containers
            criterion
            deepseq
            random
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
      vector = callPackage ({ QuickCheck, base, deepseq, ghc-prim, mkDerivation, primitive, random, stdenv, template-haskell, test-framework, test-framework-quickcheck2, transformers }:
      mkDerivation {
          pname = "vector";
          version = "0.11.0.0";
          sha256 = "1r1jlksy7b0kb0fy00g64isk6nyd9wzzdq31gx5v1wn38knj0lqa";
          revision = "2";
          editedCabalFile = "2bfafd758ab4d80fa7a16b0a650aff60fb1be109728bed6ede144baf1f744ace";
          libraryHaskellDepends = [
            base
            deepseq
            ghc-prim
            primitive
          ];
          testHaskellDepends = [
            base
            QuickCheck
            random
            template-haskell
            test-framework
            test-framework-quickcheck2
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/haskell/vector";
          description = "Efficient Arrays";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      vector-algorithms = callPackage ({ QuickCheck, base, bytestring, containers, mkDerivation, primitive, stdenv, vector }:
      mkDerivation {
          pname = "vector-algorithms";
          version = "0.7.0.1";
          sha256 = "0w4hf598lpxfg58rnimcqxrbnpqq2jmpjx82qa5md3q6r90hlipd";
          revision = "1";
          editedCabalFile = "82d67db49c85c1e136b6e6e44f99c908b405628a17b0d220c95aed34845426a5";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            bytestring
            primitive
            vector
          ];
          testHaskellDepends = [
            base
            bytestring
            containers
            QuickCheck
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://code.haskell.org/~dolio/";
          description = "Efficient algorithms for vector arrays";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      vector-binary-instances = callPackage ({ base, binary, bytestring, criterion, deepseq, mkDerivation, stdenv, tasty, tasty-quickcheck, vector }:
      mkDerivation {
          pname = "vector-binary-instances";
          version = "0.2.3.5";
          sha256 = "0niad09lbxz3cj20qllyj92lwbc013ihw4lby8fv07x5xjx5a4p1";
          libraryHaskellDepends = [
            base
            binary
            vector
          ];
          testHaskellDepends = [
            base
            binary
            tasty
            tasty-quickcheck
            vector
          ];
          benchmarkHaskellDepends = [
            base
            binary
            bytestring
            criterion
            deepseq
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/bos/vector-binary-instances";
          description = "Instances of Data.Binary and Data.Serialize for vector";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      vector-th-unbox = callPackage ({ base, data-default, mkDerivation, stdenv, template-haskell, vector }:
      mkDerivation {
          pname = "vector-th-unbox";
          version = "0.2.1.6";
          sha256 = "0d82x55f5vvr1jvaia382m23rs690lg55pvavv8f4ph0y6kd91xy";
          libraryHaskellDepends = [
            base
            template-haskell
            vector
          ];
          testHaskellDepends = [
            base
            data-default
            vector
          ];
          doHaddock = false;
          doCheck = false;
          description = "Deriver for Data.Vector.Unboxed using Template Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      versions = callPackage ({ base, either, megaparsec, microlens, mkDerivation, semigroups, stdenv, tasty, tasty-hunit, text }:
      mkDerivation {
          pname = "versions";
          version = "3.0.0";
          sha256 = "0f7wvsjavv9hkrm5pgwg99w78apsqbrw4hk559cww83k3bbbg3j6";
          libraryHaskellDepends = [
            base
            megaparsec
            semigroups
            text
          ];
          testHaskellDepends = [
            base
            either
            microlens
            tasty
            tasty-hunit
            text
          ];
          doHaddock = false;
          doCheck = false;
          description = "Types and parsers for software version numbers";
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
      wai = callPackage ({ base, blaze-builder, bytestring, bytestring-builder, hspec, http-types, mkDerivation, network, stdenv, text, transformers, vault }:
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
          testHaskellDepends = [
            base
            blaze-builder
            bytestring
            hspec
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/yesodweb/wai";
          description = "Web Application Interface";
          license = stdenv.lib.licenses.mit;
        }) {};
      wai-app-static = callPackage ({ base, blaze-builder, blaze-html, blaze-markup, bytestring, containers, cryptonite, directory, file-embed, filepath, hspec, http-date, http-types, memory, mime-types, mkDerivation, mockery, network, old-locale, optparse-applicative, stdenv, template-haskell, temporary, text, time, transformers, unix-compat, unordered-containers, wai, wai-extra, warp, zlib }:
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
          testHaskellDepends = [
            base
            bytestring
            filepath
            hspec
            http-date
            http-types
            mime-types
            mockery
            network
            old-locale
            temporary
            text
            time
            transformers
            unix-compat
            wai
            wai-extra
            zlib
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.yesodweb.com/book/web-application-interface";
          description = "WAI application for static serving";
          license = stdenv.lib.licenses.mit;
        }) {};
      wai-extra = callPackage ({ HUnit, aeson, ansi-terminal, base, base64-bytestring, blaze-builder, bytestring, case-insensitive, containers, cookie, data-default-class, deepseq, directory, fast-logger, hspec, http-types, iproute, lifted-base, mkDerivation, network, old-locale, resourcet, stdenv, streaming-commons, stringsearch, text, time, transformers, unix, unix-compat, vault, void, wai, wai-logger, word8, zlib }:
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
          testHaskellDepends = [
            base
            blaze-builder
            bytestring
            case-insensitive
            cookie
            fast-logger
            hspec
            http-types
            HUnit
            resourcet
            text
            time
            transformers
            wai
            zlib
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/yesodweb/wai";
          description = "Provides some basic WAI handlers and middleware";
          license = stdenv.lib.licenses.mit;
        }) {};
      wai-logger = callPackage ({ base, blaze-builder, byteorder, bytestring, case-insensitive, doctest, fast-logger, http-types, mkDerivation, network, stdenv, unix, unix-time, wai }:
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
          testHaskellDepends = [
            base
            doctest
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
      warp = callPackage ({ HTTP, HUnit, QuickCheck, array, async, auto-update, base, blaze-builder, bytestring, bytestring-builder, case-insensitive, containers, criterion, directory, doctest, ghc-prim, hashable, hspec, http-date, http-types, http2, iproute, lifted-base, mkDerivation, network, process, silently, simple-sendfile, stdenv, stm, streaming-commons, text, time, transformers, unix, unix-compat, vault, wai, word8 }:
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
          testHaskellDepends = [
            array
            async
            auto-update
            base
            blaze-builder
            bytestring
            bytestring-builder
            case-insensitive
            containers
            directory
            doctest
            ghc-prim
            hashable
            hspec
            HTTP
            http-date
            http-types
            http2
            HUnit
            iproute
            lifted-base
            network
            process
            QuickCheck
            silently
            simple-sendfile
            stm
            streaming-commons
            text
            time
            transformers
            unix
            unix-compat
            vault
            wai
            word8
          ];
          benchmarkHaskellDepends = [
            auto-update
            base
            bytestring
            containers
            criterion
            hashable
            http-date
            http-types
            network
            unix
            unix-compat
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
      websockets = callPackage ({ HUnit, QuickCheck, SHA, attoparsec, base, base64-bytestring, binary, blaze-builder, bytestring, case-insensitive, containers, entropy, mkDerivation, network, random, stdenv, test-framework, test-framework-hunit, test-framework-quickcheck2, text }:
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
          testHaskellDepends = [
            attoparsec
            base
            base64-bytestring
            binary
            blaze-builder
            bytestring
            case-insensitive
            containers
            entropy
            HUnit
            network
            QuickCheck
            random
            SHA
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://jaspervdj.be/websockets";
          description = "A sensible and clean way to write WebSocket-capable servers in Haskell";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      witherable = callPackage ({ base, base-orphans, containers, hashable, mkDerivation, stdenv, transformers, unordered-containers, vector }:
      mkDerivation {
          pname = "witherable";
          version = "0.1.3.4";
          sha256 = "0rqdbxl5381bfvy75bdgr8q40g1ypqgfj29ca5lzsykw5d7i4nzl";
          libraryHaskellDepends = [
            base
            base-orphans
            containers
            hashable
            transformers
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/fumieval/witherable";
          description = "filterable traversable";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      word8 = callPackage ({ base, bytestring, criterion, hspec, mkDerivation, stdenv }:
      mkDerivation {
          pname = "word8";
          version = "0.1.2";
          sha256 = "1pbn8ra3qhwvw07p375cdmp7jzlg07hgdcr4cpscz3h7b9sy7fiw";
          libraryHaskellDepends = [
            base
          ];
          testHaskellDepends = [
            base
            hspec
          ];
          benchmarkHaskellDepends = [
            base
            bytestring
            criterion
          ];
          doHaddock = false;
          doCheck = false;
          description = "Word8 library";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      wreq = callPackage ({ HUnit, QuickCheck, aeson, aeson-pretty, attoparsec, authenticate-oauth, base, base16-bytestring, base64-bytestring, byteable, bytestring, case-insensitive, containers, cryptohash, directory, doctest, exceptions, filepath, ghc-prim, hashable, http-client, http-client-tls, http-types, lens, lens-aeson, mime-types, mkDerivation, network-info, psqueues, snap-core, snap-server, stdenv, template-haskell, temporary, test-framework, test-framework-hunit, test-framework-quickcheck2, text, time, time-locale-compat, transformers, unix-compat, unordered-containers, uuid, vector }:
      mkDerivation {
          pname = "wreq";
          version = "0.5.0.1";
          sha256 = "138n138rczs5xb7pr25b5a2ajhhxph7vfrh02x71w2alh2xr4akc";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            attoparsec
            authenticate-oauth
            base
            base16-bytestring
            byteable
            bytestring
            case-insensitive
            containers
            cryptohash
            exceptions
            ghc-prim
            hashable
            http-client
            http-client-tls
            http-types
            lens
            lens-aeson
            mime-types
            psqueues
            template-haskell
            text
            time
            time-locale-compat
            unordered-containers
          ];
          testHaskellDepends = [
            aeson
            aeson-pretty
            base
            base64-bytestring
            bytestring
            case-insensitive
            containers
            directory
            doctest
            filepath
            hashable
            http-client
            http-types
            HUnit
            lens
            lens-aeson
            network-info
            QuickCheck
            snap-core
            snap-server
            temporary
            test-framework
            test-framework-hunit
            test-framework-quickcheck2
            text
            time
            transformers
            unix-compat
            unordered-containers
            uuid
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://www.serpentine.com/wreq";
          description = "An easy-to-use HTTP client library";
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
      x509 = callPackage ({ asn1-encoding, asn1-parse, asn1-types, base, bytestring, containers, cryptonite, hourglass, memory, mkDerivation, mtl, pem, stdenv, tasty, tasty-quickcheck }:
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
          testHaskellDepends = [
            asn1-types
            base
            bytestring
            cryptonite
            hourglass
            mtl
            tasty
            tasty-quickcheck
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
      yaml = callPackage ({ HUnit, aeson, aeson-qq, attoparsec, base, base-compat, bytestring, conduit, containers, directory, filepath, hspec, mkDerivation, mockery, resourcet, scientific, semigroups, stdenv, template-haskell, temporary, text, transformers, unordered-containers, vector }:
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
          testHaskellDepends = [
            aeson
            aeson-qq
            base
            base-compat
            bytestring
            conduit
            directory
            hspec
            HUnit
            mockery
            resourcet
            temporary
            text
            transformers
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/snoyberg/yaml/";
          description = "Support for parsing and rendering YAML documents";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      zlib = callPackage ({ QuickCheck, base, bytestring, mkDerivation, stdenv, tasty, tasty-hunit, tasty-quickcheck, zlib }:
      mkDerivation {
          pname = "zlib";
          version = "0.6.1.2";
          sha256 = "1fx2k2qmgm2dj3fkxx2ry945fpdn02d4dkihjxma21xgdiilxsz4";
          libraryHaskellDepends = [
            base
            bytestring
          ];
          librarySystemDepends = [ zlib ];
          testHaskellDepends = [
            base
            bytestring
            QuickCheck
            tasty
            tasty-hunit
            tasty-quickcheck
          ];
          doHaddock = false;
          doCheck = false;
          description = "Compression and decompression in the gzip and zlib formats";
          license = stdenv.lib.licenses.bsd3;
        }) { zlib = pkgs.zlib; };
      zlib-bindings = callPackage ({ QuickCheck, base, bytestring, hspec, mkDerivation, stdenv, zlib }:
      mkDerivation {
          pname = "zlib-bindings";
          version = "0.1.1.5";
          sha256 = "02ciywlz4wdlymgc3jsnicz9kzvymjw1www2163gxidnz4wb8fy8";
          revision = "2";
          editedCabalFile = "0c6f9f81832af2473281fd58631aff8c6bbad24191e00d2a5a6ae2479249043b";
          libraryHaskellDepends = [
            base
            bytestring
            zlib
          ];
          testHaskellDepends = [
            base
            bytestring
            hspec
            QuickCheck
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
  initialPackages = (args: self: (hackagePackages args self) // (stackPackages args self));
}

