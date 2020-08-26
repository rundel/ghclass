# Based on https://ropensci.org/technotes/2019/12/08/precompute-vignettes/

# Vignettes that depend on GitHub PAT are precompiled:

withr::with_dir(
  here::here("vignettes/articles"),
  {
    knitr::knit("ghclass.Rmd_raw", "ghclass.Rmd")
    knitr::knit("peer.Rmd_raw", "peer.Rmd")
    knitr::knit("peer_students.Rmd_raw", "peer_students.Rmd")
  }
)

#devtools::build_vignettes()

