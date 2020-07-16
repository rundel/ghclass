all: docs install articles pkgdown


install:
	R CMD INSTALL --no-multiarch --with-keep.source ./

docs:
	Rscript -e "devtools::document()"

pkgdown:
	Rscript -e "pkgdown::build_site(run_dont_run = TRUE, new_process = FALSE)"

pkgdown_quick:
	Rscript -e "pkgdown::build_site()"


articles:
	Rscript vignettes/articles/precompile.R

clean: clean_cache
	rm -rf doc/
	rm -rf docs/
	rm -rf vignettes/articles/figure/
	rm -f vignettes/articles/ghclass.Rmd

clean_cache:
	rm -rf vignettes/articles/cache/

.PHONY: all install docs pkgdown articles clean clean_cache
