require_git = function()
{
  git = Sys.which("git")

  if (git == "") {
    stop("git executable not found, if it is installed,",
         "please make sure git can be found using your PATH variable.")
  }

  return(git)
}



styler_available = function() {
  "styler" %in% rownames(installed.packages())
}

empty_result = function(res) {
  length(res) == 1 & all(res == "")
}



# Check for errors that result from using purrr::safely
check_errors = function(res) {
  purrr::map(res, "error") %>%
    purrr::map_lgl(is.null) %>%
    `!`()
}

# Collect errors that result from using purrr::safely
get_errors = function(res) {
  purrr::map(res, "error") %>%
    purrr::discard(is.null) %>%
    purrr::map_chr(as.character)
}

format_errors = function(errors, indent=2) {

  if (is.numeric(indent))
    indent = paste(rep(" ", indent), collapse="")

  errors %>%
    paste(indent, "*", ., collapse="") %>%
    sub("\n$", "", .)
}
