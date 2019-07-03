filter_results = function(x, ...) {
  UseMethod("filter_results", x)
}

filter_results.default = function(res, pattern = NULL, exclude = FALSE) {
  if (!is.null(pattern)) {
    subset = grepl(pattern, res)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }
  res
}

filter_results.data.frame = function(res, col, pattern = NULL, exclude = FALSE) {
  if (!is.null(pattern)) {
    subset = grepl(pattern, res[[col]])
    if (exclude) res = res[!subset,]
    else         res = res[subset,]
  }
  res
}

fix_repo_name = function(repo_name) {
  repo_name = stringr::str_replace_all(repo_name, " ", "_")
  stringr::str_replace_all(repo_name, "[^A-Za-z0-9_.-]+","-")
}




replace_nas = function(cur, rep) {
  cur[is.na(cur)] = rep[is.na(cur)]
  cur
}

require_git = function() {
  git = Sys.which("git")

  if (git == "") {
    stop("git executable not found, if it is installed,",
         "please make sure git can be found using your PATH variable.")
  }

  return(git)
}


styler_available = function() {
  "styler" %in% rownames(utils::installed.packages())
}

empty_result = function(res) {
  if (is_safely_result(res))
    empty_result(result(res))
  else
    length(res) == 1 & all(res == "")
}




format_list = function(lines, indent=2, indent_char=" ") {
  if (is.numeric(indent))
    indent = paste(rep(indent_char, indent), collapse="")

  lines = sub("\n", paste0("\n",indent, "  ") , lines)
  paste0(indent, "* ", lines, collapse="\n")
}

is_safely_result = function(res) {
  if (!is.list(res))
    return(FALSE)

  if (!all(c("result", "error") == names(res)))
    return(FALSE)

  TRUE
}



flag_experimental = function() {
  calling_func = as.character(sys.calls()[[sys.nframe()-1]])[1]

  usethis::ui_warn( paste0(
    "This function ({usethis::ui_value(calling_func)}) is currently ",
    "considered experimental. Its interface, implementation, and other ",
    "features may change significantly in future versions of the package. ",
    "Use with caution.\n"
  ) )
}


result = function(x) {
  x[["result"]]
}

error = function(x) {
  x[["error"]]
}

succeeded = function(x) {
  !is.null(result(x))
}

failed = function(x) {
  !is.null(error(x))
}

error_msg = function(x, truncate = TRUE) {
  msg = error(x)[["message"]]
  if (truncate)
    sub("\\n.*", "", msg)
  else
    msg
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


status_msg = function(x, success, fail, include_error_msg = TRUE) {
  if (succeeded(x) & !missing(success)) {
    usethis::ui_done(success)
  }

  if (failed(x) & !missing(fail)) {
    if (include_error_msg){
      fail = c(fail, "[Error: {error_msg(x)}]")
      usethis::ui_oops(fail)
    }
  }
}



