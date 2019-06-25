

# Magical ... and substitute voodoo based on
# http://adv-r.had.co.nz/Computing-on-the-language.html#substitute

handle_arg_list = function(..., tests) {
  values = list(...)
  names = eval(substitute(alist(...)))
  names = purrr::map(names, deparse)

  purrr::walk2(names, values, tests)
}

arg_is_chr_scalar = function(..., allow_null = FALSE, allow_na = FALSE) {
  arg_is_chr(..., allow_null = allow_null)
  arg_is_scalar(..., allow_null = allow_null, allow_na = allow_na)
}

arg_is_lgl_scalar = function(..., allow_null = allow_null, allow_na = allow_na) {
  arg_is_lgl(...)
  arg_is_scalar(..., allow_null = allow_null, allow_na = allow_na)
}


arg_is_chr = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.character(value) | (is.null(value) & allow_null)))
        usethis::ui_stop("Argument {usethis::ui_value(name)} must be of character type.")
    }
  )
}

arg_is_lgl = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.logical(value) | (is.null(value) & allow_null)))
        usethis::ui_stop("Argument {usethis::ui_value(name)} must be of logical type.")
    }
  )
}

arg_is_scalar = function(...,  allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (length(value) > 1 | (!allow_null & length(value) == 0)) {
        usethis::ui_stop("Argument {usethis::ui_value(name)} must be of length 1.")
      }

      if (!is.null(value)) {
        if (is.na(value) & !allow_na)
          usethis::ui_stop("Argument {usethis::ui_value(name)} must not be a missing value ({usethis::ui_value(NA)}).")
      }
    }
  )
}


