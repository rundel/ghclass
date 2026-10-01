
empty_result = function(res) {
  if (is.null(res)) {
    TRUE
  } else if (is_safely_result(res)) {
    empty_result(result(res))
  } else {
    (length(res) == 1 & all(res == "")) | (is.list(res) & length(res) == 0)
  }
}


is_safely_result = function(x) {
  if (!is.list(x))
    return(FALSE)

  if (!all(c("result", "error") %in% names(x)))
    return(FALSE)

  TRUE
}

check_safely_result = function(x) {
  if (!is_safely_result(x))
    stop(cli_glue("Object is not a {.code purrr::safely} result"), call. = FALSE)
}

#' @rdname ghclass-internal
#' @export
result = function(x) {
  #check_safely_result(x)
  x[["result"]]
}

#' @rdname ghclass-internal
#' @export
error = function(x) {
  #check_safely_result(x)
  x[["error"]]
}

#' @rdname ghclass-internal
#' @export
succeeded = function(x) {
  !is.null(result(x))
}

#' @rdname ghclass-internal
#' @export
failed = function(x) {
  !is.null(error(x))
}

# A purrr::safely() style result for failures detected before any API call
failed_result = function(msg) {
  list(result = NULL, error = simpleError(msg))
}

error_msg = function(x) {
  e = error(x)
  msg = trimws(e[["message"]])

  if (!grepl("GitHub API error", msg))
    return(msg)

  # gh >= 1.5 splits the message into a header and bullets and attaches the
  # parsed response, older versions put everything in a multiline message
  lines = trimws(unname(unlist(strsplit(c(msg, e[["body"]]), "\n"))))
  lines = lines[nzchar(lines)]

  content = e[["response_content"]]

  sub_replace = function(m, pat) {
    res = gsub(pat, "", m[grepl(pat, m)])

    if (length(res) == 0)
      NULL
    else
      res
  }

  strip_url = function(x) {
    if (is.null(x)) NULL else gsub("^<|>$", "", x)
  }

  error = gsub("\\s+", " ", lines[1])

  api_msg = content[["message"]]
  if (is.null(api_msg))
    api_msg = sub_replace(lines, "Message: ")
  if (!is.null(api_msg) && grepl(gsub("\\s+", " ", api_msg), error, fixed = TRUE))
    api_msg = NULL

  # Validation and conflict responses put the useful explanation in `errors`
  errors = purrr::map_chr(content[["errors"]], function(x) {
    if (!is.list(x))
      as.character(x)
    else if (!is.null(x[["message"]]))
      x[["message"]]
    else
      paste(c(x[["resource"]], x[["field"]], x[["code"]]), collapse = " ")
  })
  api_msg = gsub("\\s+", " ", trimws(c(api_msg, errors)))
  api_msg = api_msg[nzchar(api_msg)]
  if (length(api_msg) > 0)
    attr(error, "msg") = paste(api_msg, collapse = "; ")

  doc = content[["documentation_url"]]
  if (is.null(doc))
    doc = strip_url(sub_replace(lines, "Read more at "))
  attr(error, "doc") = doc

  attr(error, "404") = strip_url(sub_replace(lines, "URL not found: "))
  if (inherits(e, c("http_error_403", "http_error_404")))
    attr(error, "scopes") = missing_scope_hint(e[["response_headers"]])

  error
}

allow_error = function(res, message = NULL, class = NULL, result = "") {

  stopifnot(!is.null(message) | !is.null(class))

  if (succeeded(res))
    return(res)

  message_flag = TRUE
  if (!is.null(message)) {
    message_flag = grepl(message, error_msg(res))
  }

  class_flag = TRUE
  if (!is.null(class)) {
    class_flag = inherits(error(res), class)
  }

  if (message_flag & class_flag) {
    list(
      result = result,
      error = NULL
    )
  } else {
    res
  }
}

ternary = function(check, success, fail) {
  if (check)
    success
  else
    fail
}

has_doc_attr = function(x) {
  !is.null(attr(x, "doc"))
}

has_404_attr = function(x) {
  !is.null(attr(x, "404"))
}

# GitHub's secondary (abuse) rate limit on content creation is a 403 with primary
# quota remaining, which gh does not retry, and the template generate endpoint
# reports the same block as a 422. Neither response carries a retry-after header
# and the block was observed to last ~30 minutes.
is_rate_limit_error = function(e) {
  if (is.null(e))
    return(FALSE)

  if (inherits(e, "http_error_429"))
    return(TRUE)

  content = e[["response_content"]]
  msg = paste(
    c(e[["message"]], e[["body"]], content[["message"]], unlist(content[["errors"]])),
    collapse = " "
  )

  (inherits(e, "http_error_403") && grepl("secondary rate limit|abuse detection", msg, ignore.case = TRUE)) ||
    (inherits(e, "http_error_422") && grepl("submitted too quickly", msg, ignore.case = TRUE))
}

abort_rate_limited = function() {
  cli::cli_abort(
    c(
      "GitHub has temporarily blocked content creation for this token (secondary rate limit).",
      "i" = "Nothing else will succeed until the block clears, which took about 30 minutes in testing.",
      "i" = "Items completed before the block are kept, rerun the remaining ones later."
    ),
    call = NULL, class = "ghclass_rate_limit_error"
  )
}

# TODO - fix error_msg processing - doesnt work for PR and some others

#' @rdname ghclass-internal
#' @export
status_msg = function(x, success = NULL, fail = NULL, include_error_msg = TRUE,
                      .envir = parent.frame()) {

  scope = status_scope_for(.envir)

  if (succeeded(x)) {
    if (!is.null(scope))
      status_scope_event(scope, "ok", if (is.null(success)) NULL else status_text(success, .envir))
    else if (!is.null(success))
      cli::cli_alert_success(success, wrap = FALSE, .envir = .envir)
  }

  if (failed(x)) {
    if (!is.null(fail)) {
      cli::cli_alert_danger(fail, wrap = FALSE, .envir = .envir)
      if (include_error_msg) {
        msg = error_msg(x)
        cli::cli_verbatim(error_msg_tree(msg))
      }
    }
    if (!is.null(scope))
      status_scope_event(scope, "fail", if (is.null(fail)) NULL else status_text(fail, .envir))

    if (is_rate_limit_error(error(x)))
      abort_rate_limited()
  }

  invisible(x)
}

# Labeled details attached to an error by error_msg()
error_msg_details = function(msg) {
  labels = c(
    msg = "API message", doc = "API docs", "404" = "Missing page",
    scopes = "Possible missing scope"
  )
  labels = labels[names(labels) %in% names(attributes(msg))]

  details = purrr::map_chr(names(labels), ~ attr(msg, .x))
  names(details) = labels

  details
}

error_msg_tree = function(msg) {
  details = error_msg_details(msg)
  ids = ternary(length(details) > 0, paste0("detail_", seq_along(details)), character())

  extra = list()
  if (length(details) > 0)
    extra = as.list(paste0(" ", names(details), ": ", cli::col_grey(details)))

  d = data.frame(
    id = c("root", "error", ids),
    nodes = I(c(list("error", ids), rep(list(NULL), length(ids)))),
    extra = I(c(list("Error Tree", as.vector(msg)), extra)),
    stringsAsFactors = FALSE
  )

  # Let long urls and messages wrap in the console rather than being truncated
  cli::tree(d, width = 10000L)[-1]
}

# Bullets describing a failed API call, for use in cli::cli_abort() messages
error_bullets = function(x) {
  msg = error_msg(x)
  escape = function(x) gsub("\\}", "}}", gsub("\\{", "{{", x))

  details = purrr::imap_chr(error_msg_details(msg), function(value, label) {
    if (grepl("^https?://", value))
      value = paste0("{.url ", value, "}")
    else
      value = escape(value)

    paste0(label, ": ", value)
  })
  names(details) = rep("i", length(details))

  c("x" = escape(as.vector(msg)), details)
}

