#!/usr/bin/env bash

pandoc --pdf-engine=xelatex         \
       --template=template.tex      \
       -V fontsize=9pt              \
       -V classoption=oneside       \
       -V geometry="margin=1.2in"   \
       -V geometry="headsep=0.5in"  \
       -V geometry="paper=a4paper"  \
       -o tech-spec.pdf             \
       ../tech-spec.md
