rm Tables.pdf
pdflatex -synctex=1 -interaction=nonstopmode  ./Tables.tex > TablePDF.txt
if [ -e  Tables.pdf ]
then
    okular Tables.pdf 
else
    echo "Problem!!!"
fi

