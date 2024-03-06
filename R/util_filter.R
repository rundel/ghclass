filter_results = function(res, pattern = NULL, exclude = FALSE, col = NULL) {
  UseMethod("filter_results", res)
}

#' @exportS3Method
filter_results.default = function(res, pattern = NULL, exclude = FALSE, col = NULL) {
  if (is.null(pattern))
    return(res)

  subset = grepl(pattern, res)
  if (exclude)
    res[!subset]
  else
    res[subset]
}

#' @exportS3Method
filter_results.data.frame = function(res, pattern = NULL, exclude = FALSE, col = NULL) {
  if (is.null(pattern))
    return(res)

  stopifnot(!is.null(col))

  subset = grepl(pattern, res[[col]])
  if (exclude)
    res[!subset,]
  else
    res[subset,]
}
