with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      unicode-math lm-math amsmath
                      enumitem bclogo xcolor newunicodechar

                      # libraries for marginal notes
                      xargs todonotes

                      # bclogo dependencies
                      mdframed xkeyval etoolbox needspace
                      pgf

                      # build tools
                      latexmk

                      ;
                  })
                ];
}
