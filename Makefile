
default: graph.pdf

.stack-work/dist/*/*/build/depgraph/depgraph: app/*.hs lib/*.hs
	stack build

main.aux: main.tex
	latexmk -pdf -interaction=nonstopmode main

output.dot: .stack-work/dist/*/*/build/depgraph/depgraph input.txt main.tex main.aux
	stack exec depgraph input.txt output.dot

graph.pdf: output.dot
	dot -Tpdf output.dot > graph.pdf

show: output.dot
	dot -Tx11 output.dot

clean:
	stack clean
	rm -rf output.dot graph.pdf .aux *.log *.out *.snm *.toc *.vrb *.nav *.synctex.gz *.blg *.bbl *.fdb_latexmk *.fls *.ind *.idx *.ilg *.bcf *.run.xml *.xdv
