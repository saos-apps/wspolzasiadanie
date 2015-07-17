files=wspolzasiadanie
output_format=NULL

#============================================================================ 

define r-render
Rscript -e 'rmarkdown::render("$<", output_format=$(output_format))'
endef

define r-knit
Rscript -e 'knitr::knit("$<", output="$@")'
endef

.PHONY: default
default: $(files:=.html)

%.html: %.Rmd
	$(r-render)
	
%.pdf: %.Rmd
	$(r-render)

%.html: %.md
	$(r-render)

%.pdf: %.md
	$(r-render)
	

$(files:=.pdf): output_format="pdf_document"

.PHONY: edit
edit:
	vim -p wspolzasiadanie.Rmd wspolzasiadanie.bib


#============================================================================ 

wspolzasiadanie.pdf wspolzasiadanie.html: wspolzasiadanie.bib
