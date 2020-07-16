

# Magical ... and substitute voodoo based on
# http://adv-r.had.co.nz/Computing-on-the-language.html#substitute

handle_arg_list = function(..., tests) {
  values = list(...)
  names = eval(substitute(alist(...)))
  names = purrr::map(names, deparse)

  purrr::walk2(names, values, tests)
}

arg_is_chr_scalar = function(..., allow_null = FALSE, allow_na = FALSE) {
  arg_is_chr(..., allow_null = allow_null, allow_na = allow_na)
  arg_is_scalar(..., allow_null = allow_null, allow_na = allow_na)
}

arg_is_lgl_scalar = function(..., allow_null = FALSE, allow_na = FALSE) {
  arg_is_lgl(..., allow_null = allow_null, allow_na = allow_na)
  arg_is_scalar(..., allow_null = allow_null, allow_na = allow_na)
}

arg_is_pos_int_scalar = function(..., allow_null = FALSE, allow_na = FALSE) {
  arg_is_pos_int(..., allow_null = allow_null)
  arg_is_scalar(..., allow_null = allow_null, allow_na = allow_na)
}

arg_is_chr = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = TRUE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.character(value) | (is.null(value) & allow_null)))
        cli_stop("Argument {.val {name}} must be of character type.")

      if (any(is.na(value)) & !allow_na)
        cli_stop("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !allow_empty)
        cli_stop("Argument {.val {name}} must have length >= 1.")
    }
  )
}

arg_is_lgl = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = TRUE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.logical(value) | (is.null(value) & allow_null)))
        cli_stop("Argument {.val {name}} must be of logical type.")

      if (any(is.na(value)) & !allow_na)
        cli_stop("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !allow_empty)
        cli_stop("Argument {.val {name}} must have length >= 1.")
    }
  )
}

arg_is_scalar = function(...,  allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (length(value) > 1 | (!allow_null & length(value) == 0)) {
        cli_stop("Argument {.val {name}} must be of length 1.")
      }

      if (!is.null(value)) {
        if (is.na(value) & !allow_na)
          cli_stop("Argument {.val {name}} must not be a missing value ({.val {NA}}).")
      }
    }
  )
}

arg_is_raw = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.raw(value) | (is.null(value) & allow_null)))
        cli_stop("Argument {.val {name}} must be of type raw.")
    }
  )
}

arg_is_pos_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.integer(value) | (is.numeric(value) && value > 0 && value%%1 == 0) | (is.null(value) & allow_null)))
        cli_stop("Argument {.val {name}} must be a whole positive number.")
    }
  )
}

arg_is_pos_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.integer(value) | (is.numeric(value) && value > 0 && value%%1 == 0) | (is.null(value) & allow_null)))
        cli_stop("Argument {.val {name}} must be a whole positive non-zero number.")
    }
  )
}
