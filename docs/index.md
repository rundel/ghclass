# ghclass

## Tools for managing GitHub class organization accounts

This R package is designed to enable instructors to efficiently manage
their courses on GitHub. It has a wide range of functionality for
managing organizations, teams, repositories, and users on GitHub and
helps automate most of the tedious and repetitive tasks around creating
and distributing assignments.

Install ghclass from CRAN:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"ghclass"``)`

Install the development version package from GitHub:

\
`# install.packages("remotes")`\
`remotes``::`[`install_github`](https://remotes.r-lib.org/reference/install_github.html)`(``"rundel/ghclass"``)`

See package
[vignette](https://rundel.github.io/ghclass/articles/ghclass.html) for
details on how to use the package.

## Peer Review

The peer review functionality currently lives on the `peer_review`
branch and is not part of the CRAN release. If you need it you can
install that branch using:

\
`remotes``::`[`install_github`](https://remotes.r-lib.org/reference/install_github.html)`(``"rundel/ghclass@peer_review"``)`

## GitHub & default branches

GitHub now uses `main` as the default branch for new repositories (see
[here](https://github.com/github/renaming) for background). `ghclass`
supports alternative default branch names across the entire package, so
for the vast majority of use cases you will not need to do anything
differently. See the FAQ in the Getting Started vignette for more
details.
