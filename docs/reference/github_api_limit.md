# Tools for limiting gh's GitHub api requests.

- `github_get_api_limit()` - returns the current limit on results
  returned by gh.

- `github_set_api_limit()` - sets a limit on results returned by gh.

- `github_get_max_wait()` / `github_set_max_wait()` - get or set the
  maximum number of seconds gh will wait when it hits a rate limit.
  `NULL` (the default) uses gh's own default.

- `github_get_max_rate()` / `github_set_max_rate()` - get or set the
  maximum request rate, in requests per second, used to proactively
  throttle requests and avoid secondary rate limits. `NULL` (the
  default) disables throttling.

## Usage

``` r
github_get_api_limit()

github_set_api_limit(limit = 10000L)

github_get_max_wait()

github_set_max_wait(max_wait = NULL)

github_get_max_rate()

github_set_max_rate(max_rate = NULL)
```

## Arguments

- limit:

  The maximum number of records to return from an API request.

- max_wait:

  The maximum number of seconds to wait when rate limited, or `NULL` to
  use gh's default. Passed to
  [`gh::gh()`](https://gh.r-lib.org/reference/gh.html)'s `.max_wait`.

- max_rate:

  The maximum request rate in requests per second, or `NULL` for no
  throttling. Passed to
  [`gh::gh()`](https://gh.r-lib.org/reference/gh.html)'s `.max_rate`.

## Value

`github_get_api_limit()` returns a single integer value.

`github_get_max_wait()` and `github_get_max_rate()` return their stored
value or `NULL`.

`github_set_*()` functions invisibly return the value of their argument.

## Details

These values are stored in the `"ghclass.api.limit"`,
`"ghclass.max.wait"`, and `"ghclass.max.rate"` options globally.

## Examples

``` r
github_get_api_limit()
#> [1] 10000

github_set_api_limit(500)

github_get_api_limit()
#> [1] 500
```
