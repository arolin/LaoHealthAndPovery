# RMDFILE=MainAnalysis
RMDFILE=CostAnalysis MNCH_Analysis UsageAnalysis MainAnalysis OOPCross_Anlysis


%: %.rmd
	Rscript -e "require(knitr); require(markdown); rmarkdown::render('$@.rmd',output_dir='./html')"


all: $(RMDFILE)


## Rscript -e "require(knitr); require(markdown); knit('$(RMDFILE).rmd', '$(RMDFILE).md'); markdownToHTML('$(RMDFILE).md', '$(RMDFILE).html', stylesheet='custom.css')"
