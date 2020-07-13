# Based on https://ropensci.org/technotes/2019/12/08/precompute-vignettes/

# Vignettes that depend on GitHub PAT are precompiled:

withr::with_dir(
  here::here("vignettes/"),
  {
    print(getwd())
    knitr::knit(".raw/ghclass.Rmd", "ghclass.Rmd")
    knitr::knit(".raw/peer_students.Rmd", "peer_students.Rmd")
    #knitr::knit(".raw/peer.Rmd", "peer.Rmd")
  }
)



devtools::build_vignettes()
