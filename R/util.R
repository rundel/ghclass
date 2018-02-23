require_git = function()
{
  git = Sys.which("git")

  if (git == "") {
    stop("git executable not found, if it is installed,",
         "please make sure git can be found using your PATH variable.")
  }

  return(git)
}

safe_gh = purrr::safely(gh)

styler_available = function() {
  "styler" %in% rownames(utils::installed.packages())
}

empty_result = function(res) {
  length(res) == 1 & all(res == "")
}

listify_result = function(res) {
  if (all(c("error","result") %in% names(res)))
    list(res)
  else
    res
}

# Check for errors that result from using purrr::safely
check_errors = function(res) {
  errs = purrr::map(res, "error")
  purrr::map_lgl(errs, ~!is.null(.x))
}

# Collect errors that result from using purrr::safely
get_errors = function(res) {
  errs = purrr::map(res, "error")
  errs = purrr::discard(errs, is.null)
  errs = purrr::map_chr(errs, conditionMessage)
  errs = sub("\n$", "", errs)
  errs = sub("[[:space:]]*\n[[:space:]]*"," - ", errs)
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
  if (length(res) == 0)
    return(FALSE)
  if (!all(purrr::map_int(res, length) == 2))
    return(FALSE)

  names = purrr::map(res, names)
  names = sort(unique(unlist(names)))
  if (!all(c("error","result") == names))
    return(FALSE)

  TRUE
}

# Expects one or more results from purrr::safely
check_result = function(res, fail_msg, verbose=FALSE, error_prefix = "") {
  res = listify_result(res)
  stopifnot(is_safely_result(res))

  errs = check_errors(res)
  if (length(error_prefix) == length(res))
    error_prefix = error_prefix[errs]

  if (any(errs)) {
    if (verbose)
      fail_msg = paste0(
        fail_msg,
        "\n",
        format_list(
          paste0(error_prefix, get_errors(res))
        )
      )

    warning(fail_msg, call. = FALSE, immediate. = TRUE)
  }
}
