github_api_v4_graphql = function(query, vars = list()) {
  arg_is_chr_scalar(query)

  query = graphql_glue(query, vars)

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

  if (code >= 300) {
    cli_stop("Github API v4 error code ({code}) - {res[['message']]}")
  }

  res
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

    if (!is.null(res[[i]]$errors)) {
      msgs = purrr::map(res[[i]]$errors, "message")
      msg = paste(unlist(msgs), collapse="\n")
      cli_stop(msg)
    }

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
