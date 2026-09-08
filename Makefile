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
	Rscript -e "pkgdown::build_article('articles/ghclass', quiet = FALSE)"

clean:
	rm -rf doc/
	rm -rf docs/
	rm -rf vignettes/.quarto/

.PHONY: all install docs pkgdown pkgdown_quick articles clean
