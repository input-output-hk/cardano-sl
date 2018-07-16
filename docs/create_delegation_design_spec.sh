#1/bin/bash

gnuplot rewards.gnuplot
pandoc -f markdown -t latex -o delegation_design_spec.pdf delegation_design_spec.md
