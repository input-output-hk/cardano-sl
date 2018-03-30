#!/usr/bin/env bash

pandoc --pdf-engine=xelatex    \
       --template=template.tex \
       -o report.pdf           \
       ../report.md
