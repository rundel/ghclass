all: docs articles pkgdown

docs:
	Rscript -e "devtools::document()"

pkgdown:
	Rscript -e "pkgdown::build_site(run_dont_run = TRUE, new_process = FALSE)"

articles:
	Rscript vignettes/articles/precompile.R

clean:
	rm -rf doc/
	rm -rf docs/
	rm -rf vignettes/articles/cache/
	rm -rf vignettes/articles/figure/
	rm -f vignettes/articles/ghclass.Rmd

.PHONY: all docs pkgdown articles clean
