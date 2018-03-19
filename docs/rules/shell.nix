with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      unicode-math lm-math amsmath
                      enumitem
                      # libraries for marginal notes
                      xargs todonotes

                      # build tools
                      latexmk

                      ;
                  })
                ];
}
