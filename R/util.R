styler_available = function() {
  "styler" %in% rownames(installed.packages())
}

empty_result = function(res) {
  length(res) == 1 & all(res == "")
}
