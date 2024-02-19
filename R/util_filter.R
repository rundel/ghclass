filter_results = function(res, col, pattern = NULL, exclude = FALSE) {
  UseMethod("filter_results", res)
}

#' @exportS3Method
filter_results.default = function(res, col, pattern = NULL, exclude = FALSE) {
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
