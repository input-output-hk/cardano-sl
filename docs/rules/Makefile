# See `./latexmkrc` for latexmk configuration
LATEX	= latexmk
PVC	= -pvc

F.tex	= $(wildcard *.tex)
F.pdf	= $(F.tex:%.tex=%.pdf) $(F.lhs:%.lhs=%.pdf)
F	= $(F.tex:%.tex=%) $(F.lhs:%.lhs=%)

.PHONY: default
default: all

.PHONY: all
all: $(F.pdf)

%.pdf: %.tex
	$(LATEX) $<

$(F):
	$(LATEX) $(PVC) -use-make $@.tex
