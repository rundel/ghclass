# Based on https://ropensci.org/technotes/2019/12/08/precompute-vignettes/

# Vignettes that depend on GitHub PAT are precompiled:

knitr::knit("vignettes/.raw/ghclass.Rmd", "vignettes/ghclass.Rmd")
knitr::knit("vignettes/.raw/instructions_students.Rmd.orig", "vignettes/instructions_students.Rmd")
knitr::knit("vignettes/.raw/peer.Rmd.orig", "vignettes/peer.Rmd")

devtools::build_vignettes()
