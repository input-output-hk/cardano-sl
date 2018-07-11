spec.pdf:
	pdflatex -shell-escape spec.tex
	bibtex spec
	pdflatex -shell-escape spec.tex
	pdflatex -shell-escape spec.tex

watch:
	while inotifywait -e close_write spec.tex ; do pdflatex -shell-escape -halt spec.tex ; done

.PHONY: clean
clean:
	rm -f spec-figure* *.log *.out *.aux *.auxlock *.lof *.pdf *.toc
