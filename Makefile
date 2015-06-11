# RMDFILE=MainAnalysis
RMDFILE=OOPCross_Anlysis
RMDFILE=CostAnalysis


html :
	Rscript -e "require(knitr); require(markdown); rmarkdown::render('$(RMDFILE).rmd')"

## Rscript -e "require(knitr); require(markdown); knit('$(RMDFILE).rmd', '$(RMDFILE).md'); markdownToHTML('$(RMDFILE).md', '$(RMDFILE).html', stylesheet='custom.css')"
