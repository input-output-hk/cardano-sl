{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "acid-state-exts"; version = "0.14.2"; };
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
          (hsPkgs.acid-state)
          (hsPkgs.base)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.exceptions)
          (hsPkgs.extra)
          (hsPkgs.mtl)
          (hsPkgs.safecopy)
          (hsPkgs.time-units)
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs.Win32);
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././acid-state-exts; }