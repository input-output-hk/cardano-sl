# This provides a function like pkgs.haskell.lib.justStaticExecutables,
# but which also stamps the executables with the git revision.

{ pkgs, ghc, gitrev, stdenv }:

with pkgs.lib;

let
  # Wraps a haskell package derivation so that it has all dynamic
  # linking, development and doc files removed.
  #
  # It uses file-embed to inject the git revision into all programs of
  # all derivation outputs.
  #
  # This is done by copying all files from the wrapped derivation
  # rather than with a package override. If an override were used, the
  # derivation would need to be rebuilt whenever gitrev changed.
  justStaticExecutablesGitRev = drvOut: let
    drvOutOutputs = drvOut.outputs or ["out"];
  in
    pkgs.runCommand (staticName drvOut) {
      outputs  = drvOutOutputs;
      passthru = drvOut.drvAttrs
        // (drvOut.passthru or {})
        // { inherit gitrev; };
    }
    (concatMapStrings (output: ''
      cp -a "${drvOut.${output}}" "${"$"}${output}"
      chmod -R +w "${"$"}${output}"
      for prog in "${"$"}${output}"/bin/*; do
        ${setGitRev} "${gitrev}" "$prog" || true
      done
      rm -rf "${"$"}${output}"/lib "${"$"}${output}"/nix-support "${"$"}${output}"/share/doc
    '') drvOutOutputs);

  setGitRev = pkgs.runCommand "set-git-rev" {
    # https://github.com/NixOS/nixpkgs/issues/46814
    flags = optionalString stdenv.isDarwin "-liconv";
  } ''
    ${ghc.withPackages (ps: [ps.universum ps.file-embed])}/bin/ghc $flags -o $out ${./set-git-rev.hs}
  '';

  # appends -static to the package name
  staticName = drv: drv.pname + "-static-" + drv.version;

in
  justStaticExecutablesGitRev
