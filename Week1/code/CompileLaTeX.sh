#!/bin/bash
# Author: Xuan Wang xuan.wang22@imperial.ac.uk
#!/bin/bash
#!/bin/bash
#!/bin/bash

x=${1%.tex} #${1%.tex} expands to text
pdflatex $x.tex
bibtex $x.aux
pdflatex $x.tex
pdflatex $x.tex
evince $x.pdf &

#cleanup
rm *.aux
rm *.bbl
rm *.blg
rm *.log
echo "PDF generated"

