# ghclass (development version)

# ghclass 0.2.0

* First official release of ghclass

* Switched underlying ui tools to use cli instead of usethis

* Major cleanup and rewrite of many functions

* Basic support for GitHub actions added

* `peer_*` functions are moved to the peer_review branch for now, coming back in 0.2.1. See README for details if you need them before the next release.

* The use of "master" as the default value for branch arguments has been removed across the package. Most functions with a branch argument will continue to function in the same way and will use whatever is defined as the default branch for that repository. In a small handful of cases where the API does not allow this, the argument will now require the user to provide an explicit value. These changes were made in anticipation of GitHub moving the default branch from "master" to "main" in October (see https://github.com/github/renaming for details).
