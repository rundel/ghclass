# `withr`-like functions for temporary personal access token

Temporarily change the `GITHUB_PAT` environmental variable for GitHub
authentication. Based on the `withr` interface.

## Usage

``` r
with_pat(new, code)

local_pat(new, .local_envir = parent.frame())
```

## Arguments

- new:

  Temporary GitHub access token

- code:

  Code to execute with the temporary token

- .local_envir:

  The environment to use for scoping.

## Value

The results of the evaluation of the code argument.

## Details

if `new = NA` is used the `GITHUB_PAT` environment variable will be
unset.

## Examples

``` r
if (FALSE) { # \dontrun{
with_pat("1234", print(github_get_token()))
} # }
```
