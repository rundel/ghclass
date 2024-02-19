filter_results = function(x, ...) {
  UseMethod("filter_results", x)
}

#' @exportS3Method
filter_results.default = function(res, pattern = NULL, exclude = FALSE) {
  if (!is.null(pattern)) {
    subset = grepl(pattern, res)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }
  res
}

#' @exportS3Method
filter_results.data.frame = function(res, col, pattern = NULL, exclude = FALSE) {
  if (!is.null(pattern)) {
    subset = grepl(pattern, res[[col]])
    if (exclude) res = res[!subset,]
    else         res = res[subset,]
  }
  res
}
