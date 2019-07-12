{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "hedgehog"; version = "1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Jacob Stanley";
      homepage = "https://hedgehog.qa";
      url = "";
      synopsis = "Release with confidence.";
      description = "<http://hedgehog.qa/ Hedgehog> automatically generates a comprehensive array\nof test cases, exercising your software in ways human testers would never\nimagine.\n\nGenerate hundreds of test cases automatically, exposing even the\nmost insidious of corner cases. Failures are automatically simplified, giving\ndevelopers coherent, intelligible error messages.\n\nTo get started quickly, see the <https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-example examples>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.ansi-terminal)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.concurrent-output)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.exceptions)
          (hsPkgs.fail)
          (hsPkgs.lifted-async)
          (hsPkgs.mmorph)
          (hsPkgs.monad-control)
          (hsPkgs.mtl)
          (hsPkgs.pretty-show)
          (hsPkgs.primitive)
          (hsPkgs.random)
          (hsPkgs.resourcet)
          (hsPkgs.semigroups)
          (hsPkgs.stm)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.transformers-base)
          (hsPkgs.wl-pprint-annotated)
          ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.hedgehog)
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.mmorph)
            (hsPkgs.mtl)
            (hsPkgs.pretty-show)
            (hsPkgs.semigroups)
            (hsPkgs.text)
            (hsPkgs.transformers)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/hedgehogqa/haskell-hedgehog.git";
      rev = "a20005f4d584f19ea05524320f1c9a90a44c81db";
      sha256 = "1lca47ilzg75ibv2qbb1pl466rba6ir8ra0cg4qlji63dsbpkiks";
      });
    postUnpack = "sourceRoot+=/hedgehog; echo source root reset to \$sourceRoot";
    }