# RMDFILE=MainAnalysis
RMDFILE=OOPCross_Anlysis CostAnalysis


%: %.rmd
	Rscript -e "require(knitr); require(markdown); rmarkdown::render('$@.rmd',output_dir='./html')"



## Rscript -e "require(knitr); require(markdown); knit('$(RMDFILE).rmd', '$(RMDFILE).md'); markdownToHTML('$(RMDFILE).md', '$(RMDFILE).html', stylesheet='custom.css')"
