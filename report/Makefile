all:
			pdflatex main.tex
			bibtex main.aux
			pdflatex main.tex
			pdflatex main.tex
			make clean

clean:
			rm *.log
			rm *.aux
			# rm -r settings/*.log
			# rm -r settings/*.aux
			# rm -r mainparts/*.log
			# rm -r mainparts/*.aux
			rm *.bbl
			rm *.blg
			rm *.toc
			rm *.out
