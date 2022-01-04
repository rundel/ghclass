## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

This is a resubmission. In this version we have:

* Removed LICENSE file and corrected the relevant entry in DESCRIPTION.

* Vignette example was altered to use `tempdir()` for all downloaded files.

* All `local_repo_*` functions were checked to confirm that no default directories were used and all documentation examples exclusively use `tempdir()`

* All function documentation was reviewed and revised to make sure they include details on return values.

