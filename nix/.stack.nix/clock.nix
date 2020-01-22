let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { llvm = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "clock"; version = "0.8"; };
      license = "BSD-3-Clause";
      copyright = "Copyright © Cetin Sert 2009-2016, Eugene Kirpichov 2010, Finn Espen Gundersen 2013, Gerolf Seitz 2013, Mathieu Boespflug 2014 2015, Chris Done 2015, Dimitri Sabadie 2015, Christian Burger 2015, Mario Longobardi 2016";
      maintainer = "Cetin Sert <cetin@corsis.tech>, Corsis Research";
      author = "Cetin Sert <cetin@corsis.tech>, Corsis Research";
      homepage = "https://github.com/corsis/clock";
      url = "";
      synopsis = "High-resolution clock functions: monotonic, realtime, cputime.";
      description = "A package for convenient access to high-resolution clock and\ntimer functions of different operating systems via a unified API.\n\nPOSIX code and surface API was developed by Cetin Sert in 2009.\n\nWindows code was contributed by Eugene Kirpichov in 2010.\n\nFreeBSD code was contributed by Finn Espen Gundersen on 2013-10-14.\n\nOS X code was contributed by Gerolf Seitz on 2013-10-15.\n\nDerived @Generic@, @Typeable@ and other instances for @Clock@ and @TimeSpec@ was contributed by Mathieu Boespflug on 2014-09-17.\n\nCorrected dependency listing for @GHC < 7.6@ was contributed by Brian McKenna on 2014-09-30.\n\nWindows code corrected by Dimitri Sabadie on 2015-02-09.\n\nAdded @timeSpecAsNanoSecs@ as observed widely-used by Chris Done on 2015-01-06, exported correctly on 2015-04-20.\n\nImported Control.Applicative operators correctly for Haskell Platform on Windows on 2015-04-21.\n\nUnit tests and instance fixes by Christian Burger on 2015-06-25.\n\nRemoval of fromInteger : Integer -> TimeSpec by Cetin Sert on 2015-12-15.\n\nNew Linux-specific Clocks: MonotonicRaw, Boottime, MonotonicCoarse, RealtimeCoarse by Cetin Sert on 2015-12-15.\n\nReintroduction fromInteger : Integer -> TimeSpec by Cetin Sert on 2016-04-05.\n\nFixes for older Linux build failures introduced by new Linux-specific clocks by Mario Longobardi on 2016-04-18.\n\nRefreshment release in 2019-04 after numerous contributions.\n\n[Version Scheme]\nMajor-@/R/@-ewrite . New-@/F/@-unctionality . @/I/@-mprovementAndBugFixes . @/P/@-ackagingOnly\n\n* @PackagingOnly@ changes are made for quality assurance reasons.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "7.6") [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."clock" or (buildDepError "clock"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."clock" or (buildDepError "clock"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/corsis/clock.git";
      rev = "98ff9a0d9aa61382dc8dcda5f74943f5f0029e1a";
      sha256 = "17dcq6wyf4jmrlhkhsgiz5rwsr6fzcdfnvclln8g2da29d710bgl";
      });
    }