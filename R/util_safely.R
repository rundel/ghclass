empty_result = function(res) {
  if (is_safely_result(res)){
    empty_result(result(res))
  } else {
    length(res) == 1 & all(res == "")
  }
}


is_safely_result = function(res) {
  if (!is.list(res))
    return(FALSE)

  if (!all(c("result", "error") %in% names(res)))
    return(FALSE)

  TRUE
}

result = function(x) {
  stopifnot("result" %in% names(x))
  x[["result"]]
}

error = function(x) {
  stopifnot("error" %in% names(x))
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
