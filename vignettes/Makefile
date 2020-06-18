all: ghclass.Rmd instructions_students.Rmd peer.Rmd

ghclass.Rmd: ghclass.Rmd.orig
	Rscript -e "knitr::knit(here::here('vignettes/ghclass.Rmd.orig'), here::here('vignettes/ghclass.Rmd'))"

instructions_students.Rmd: instructions_students.Rmd.orig
	Rscript -e "knitr::knit('vignettes/instructions_students.Rmd.orig', 'vignettes/instructions_students.Rmd')"

peer.Rmd: peer.Rmd.orig
	Rscript -e "knitr::knit('vignettes/peer.Rmd.orig', 'vignettes/peer.Rmd')"
