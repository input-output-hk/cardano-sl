{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { skip-state-machine-test = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "acid-state"; version = "0.15.0"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "Lemmih <lemmih@gmail.com>";
      author = "David Himmelstrup";
      homepage = "https://github.com/acid-state/acid-state";
      url = "";
      synopsis = "Add ACID guarantees to any serializable Haskell data structure.";
      description = "Use regular Haskell data structures as your database and get stronger ACID guarantees than most RDBMS offer.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.array)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cereal)
          (hsPkgs.containers)
          (hsPkgs.extensible-exceptions)
          (hsPkgs.safecopy)
          (hsPkgs.stm)
          (hsPkgs.directory)
          (hsPkgs.filelock)
          (hsPkgs.filepath)
          (hsPkgs.mtl)
          (hsPkgs.template-haskell)
          (hsPkgs.th-expand-syns)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
        };
      tests = {
        "specs" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.acid-state)
            (hsPkgs.deepseq)
            (hsPkgs.hspec)
            (hsPkgs.hspec-discover)
            (hsPkgs.mtl)
            (hsPkgs.safecopy)
            (hsPkgs.template-haskell)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        "state-machine" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.acid-state)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.hedgehog)
            (hsPkgs.mtl)
            (hsPkgs.safecopy)
            ];
          };
        };
      benchmarks = {
        "loading-benchmark" = {
          depends = [
            (hsPkgs.random)
            (hsPkgs.directory)
            (hsPkgs.system-fileio)
            (hsPkgs.system-filepath)
            (hsPkgs.criterion)
            (hsPkgs.mtl)
            (hsPkgs.base)
            (hsPkgs.acid-state)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/avieth/acid-state";
      rev = "7c00aeeeb731f5c98fe1104693c0136b384cb824";
      sha256 = "08ncrjdqdxw7wmf74v6k8j9kk7w86q7na1smcfqszv23hg5y4agr";
      });
    }