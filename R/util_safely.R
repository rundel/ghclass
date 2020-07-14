
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

result = function(x) {
  #check_safely_result(x)
  x[["result"]]
}

error = function(x) {
  #check_safely_result(x)
  x[["error"]]
}

succeeded = function(x) {
  !is.null(result(x))
}

failed = function(x) {
  !is.null(error(x))
}

any_failed = function(x) {
  any(purrr::map_lgl(x, failed))
}

return_on_any_failed = function(x) {
  if (any_failed(x))
    do.call(return, list(), envir = sys.frame(-1))
}


error_msg = function(x) {
  msg = error(x)[["message"]]
  msg = gsub("\\n", " ", msg)

  if (grepl("GitHub API error", msg)) {
    # Handle GitHub API error messages to be a bit nicer looking
    pre = sub("Message: .*", "", msg)
    suf = sub(".* Message: ", "", msg)



    msg = paste0(pre, " (", cli::col_grey(suf), ")")
  }

  trimws(msg)
}


error_msg = function(x) {
  msg = trimws( error(x)[["message"]] )

  if (grepl("GitHub API error", msg)) {
    msg = gsub("\n+", "\n", msg)
    msg = strsplit(msg, "\n")[[1]]

    sub_replace = function(m, pat) {
      res = gsub(pat, "", m[grepl(pat, m)])

      if(length(res) == 0)
        NULL
      else
        res
    }

    error = msg[1]

    attr(error, "msg") = sub_replace(msg, "Message: ")
    attr(error, "doc") = sub_replace(msg, "Read more at ")
    attr(error, "404") = sub_replace(msg, "URL not found: ")

    error
  } else {
    msg
  }
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
  !is.null(attr(x, "doc"))
}

# TODO - fix error_msg processing - doesnt work for PR and some others

status_msg = function(x, success = NULL, fail = NULL, include_error_msg = TRUE,
                      .envir = parent.frame()) {


  if (succeeded(x) & !is.null(success)) {
    cli::cli_alert_success(success, wrap = FALSE, .envir = .envir)
  }

  if (failed(x) & !is.null(fail)) {
    cli::cli_alert_danger(fail, wrap = FALSE, .envir = .envir)
    if (include_error_msg) {
      msg = error_msg(x)
      cli::cat_line(error_msg_tree(msg))
    }
  }
}

error_msg_tree = function(msg) {
  # Only use the attrs that are provided
  attrs = names(attributes(msg))

  d = data.frame(
    id = c("root", "error", "msg", "doc", "404"),
    nodes = I(list("error", attrs, NULL, NULL, NULL)),
    extra = I(list(
      "Error Tree",
      msg,
      paste0(" ", cli_glue('API message: {cli::col_grey(attr(msg,"msg"))}')),
      paste0(" ", cli_glue('API docs: {cli::col_grey(attr(msg,"doc"))}')),
      paste0(" ", cli_glue('Missing page: {cli::col_grey(attr(msg,"404"))}'))
    )),
    stringsAsFactors = FALSE
  )

  cli::tree(d)[-1]
}


