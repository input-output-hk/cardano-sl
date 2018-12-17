with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      unicode-math lm-math amsmath
                      enumitem bclogo xcolor newunicodechar
                      appendix syntax

                      # build tools
                      latexmk

                      ;
                  })
                  cddl
                  cbor-diag
                ];
}
