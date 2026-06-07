github_api_v4_graphql = function(query, vars = list(), max_retries = 3L,
                                 max_wait = github_get_max_wait()) {
  arg_is_chr_scalar(query)

  query = graphql_glue(query, vars)
  cap = if (is.null(max_wait)) 600 else max_wait

  attempt = 0
  repeat {
    req = httr::POST(
      "https://api.github.com/graphql",
      httr::add_headers(
        Authorization = paste("bearer", github_get_token())
      ),
      encode = "json",
      body = list(query = query)
    )

    res = httr::content(req)
    code = httr::status_code(req)

    wait = graphql_rate_limit_wait(req, res, code)

    if (is.null(wait) || attempt >= max_retries)
      break

    attempt = attempt + 1
    wait = min(max(wait, 0), cap)
    cli::cli_alert_info(
      "GitHub API v4 rate limit reached; waiting {round(wait)}s before retry {attempt}/{max_retries}."
    )
    Sys.sleep(wait)
  }

  if (code >= 300) {
    cli_stop("GitHub API v4 error code ({code}) - {res[['message']]}")
  }

  if (!is.null(res$errors)) {
    msgs = unlist(purrr::map(res$errors, "message"))
    cli_stop(paste(msgs, collapse = "\n"))
  }

  res
}

# Number of seconds to wait before retrying a rate-limited v4 request, or NULL
# if the response is not a rate-limit response. GitHub signals GraphQL rate
# limits either with a 403/429 status (secondary limits, usually carrying a
# Retry-After header) or a 200 with a RATE_LIMITED error (the points budget,
# with x-ratelimit-reset).
graphql_rate_limit_wait = function(req, res, code) {
  err_types = purrr::map_chr(res[["errors"]], "type", .default = NA_character_)
  rate_limited = code %in% c(403L, 429L) || any(err_types == "RATE_LIMITED", na.rm = TRUE)

  if (!rate_limited)
    return(NULL)

  hdr = httr::headers(req)

  retry_after = hdr[["retry-after"]]
  if (!is.null(retry_after))
    return(as.numeric(retry_after))

  remaining = hdr[["x-ratelimit-remaining"]]
  reset = hdr[["x-ratelimit-reset"]]
  if (!is.null(remaining) && remaining == "0" && !is.null(reset))
    return(as.numeric(reset) - as.numeric(Sys.time()))

  5
}

github_api_v4_graphql_paginated = function(query, page_info, cursor_var = "cursor") {
  arg_is_chr_scalar(query)
  arg_is_chr(page_info)

  vars = list()
  vars[cursor_var] = list(NULL)

  page_info = as.list(page_info)
  if (page_info[[1]] != "data")
    page_info = c("data",page_info)
  if (page_info[[length(page_info)]] != "pageInfo")
    page_info = c(page_info, "pageInfo")


  res = list()
  i = 1

  repeat {
    res[[i]] = github_api_v4_graphql(query, vars)
    page = purrr::pluck(res[[i]], !!!page_info)

    if (is.null(page)) {
      cli_stop("Unable to locate page info for this query.")
    }

    if (!page$hasNextPage)
      break

    vars[[cursor_var]] = page$endCursor
    i = i + 1
  }

  res
}


graphql_glue = function(query, vars) {
  glue::glue_data(vars, query, .open = "<", .close = ">")
}

graphql_quote = function(x) {
  if (is.null(x)) {
    "null"
  } else if (is.na(x)) {
    cli_stop("{.val NA} values are not supported by graphql.")
  } else {
    glue::double_quote(x)
  }
}

graphql_escape = function(x) {
  if (is.null(x))
    return(x)

  x = gsub("\\", "\\\\", x, fixed = TRUE)
  gsub('"', '\\"', x, fixed = TRUE)
}
