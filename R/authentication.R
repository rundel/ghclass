#' Get github token
#'
#' \code{get_github_token} obtains the user's github authentication token.
#'
#' This function looks for the token in the following places (in order):
#' \enumerate{
#'   \item Value of \code{github_token} variable in \code{.ghclass} environment
#'   (this is where the package caches the token).
#'
#'   \item Value of \code{GITHUB_TOKEN} environmental variable.
#'
#'   \item Contents of \code{~/.github/token} file.
#' }
#'
#' @examples
#' \dontrun{
#' get_github_token()
#' }
#'
#' @family authentication functions
#'
#' @export
#'
get_github_token = function()
{
  token = get("github_token", envir=.ghclass)
  if (!is.null(token))
    return(token)

  token = Sys.getenv("GITHUB_TOKEN")
  if (token != "")
  {
    assign("github_token", token, envir=.ghclass)
    return(token)
  }

  if (file.exists("~/.github/token"))
  {
    set_github_token("~/.github/token")
    return(get_github_token())
  }

  stop("Unable to locate github token, please use set_github_token",
       " or define the GITHUB_TOKEN environmental variable.")
}


#' Set github token
#'
#' \code{set_github_token} defines the user's github authentication token,
#' this value is then accessed usin \code{get_github_token}
#'
#' @param token character, either the path of a file contained the token or the actual token.
#'
#' @examples
#' \dontrun{
#' set_github_token("~/.github/token")
#' set_github_token("0123456789ABCDEF0123456789ABCDEF01234567")
#' }
#'
#' @family authentication functions
#'
#' @export
#'
set_github_token = function(token)
{
  stopifnot(!missing(token))
  stopifnot(is.character(token))

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  assign("github_token", token, envir=.ghclass)
}

#' Test github token
#'
#' \code{test_github_token} checks if a token is valid by attempting to authenticate with the GitHub api.
#'
#' @param token character or missing, if missing the token is obtained using \code{get_github_token}.
#'
#' @examples
#' \dontrun{
#' test_github_token()
#' test_github_token("bad_token")
#' }
#'
#' @family authentication functions
#'
#' @export
#'
test_github_token = function(token)
{
  if (missing(token))
    token = get_github_token()

  gh("/user", .token=token)
  invisible(NULL)
}




#' Get wercker token
#'
#' \code{get_wercker_token} obtains the user's wercker authentication token.
#'
#' This function looks for the token in the following places (in order):
#' \enumerate{
#'   \item Value of \code{wercker_token} variable in \code{.ghclass} environment
#'   (this is where the package caches the token).
#'
#'   \item Value of \code{WERCKER_TOKEN} environmental variable.
#'
#'   \item Contents of \code{~/.wercker/token} file.
#' }
#'
#' @examples
#' \dontrun{
#' get_wercker_token()
#' }
#'
#' @family authentication functions
#'
#' @export
#'
get_wercker_token = function()
{
  token = get("wercker_token", envir=.ghclass)
  if (!is.null(token))
    return(token)

  token = Sys.getenv("WERCKER_TOKEN")
  if (token != "")
  {
    assign("wercker_token", token, envir=.ghclass)
    return(token)
  }

  if (file.exists("~/.wercker/token"))
  {
    set_wercker_token("~/.wercker/token")
    return(get_wercker_token())
  }

  stop("Unable to locate wercker token, please use set_wercker_token or
         define the WERCKER_TOKEN environmental variable.")
}


#' Set wercker token
#'
#' \code{set_wercker_token} defines the user's wercker authentication token,
#' this value is then accessed usin \code{get_wercker_token}
#'
#' @param token character, either the path of a file contained the token or the actual token.
#'
#' @examples
#' \dontrun{
#' set_wercker_token("~/.wercker/token")
#' set_wercker_token("0123456789ABCDEF0123456789ABCDEF01234567")
#' }
#'
#' @family authentication functions
#'
#' @export
#'
set_wercker_token = function(token)
{
  stopifnot(!missing(token))
  stopifnot(is.character(token))

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  assign("wercker_token", token, envir=.ghclass)
}


#' Test wercker token
#'
#' \code{test_wercker_token} checks if a token is valid by attempting to authenticate with the Wercker's api.
#'
#' @param token character or missing, if missing the token is obtained using \code{get_wercker_token}.
#'
#' @examples
#' \dontrun{
#' test_wercker_token()
#' test_wercker_token("bad_token")
#' }
#'
#' @family authentication functions
#'
#' @export
#'
test_wercker_token = function(token)
{
  if (missing(token))
    token = get_wercker_token()

  req = httr::GET(
    paste0("https://app.wercker.com/api/v2/profile"),
    httr::add_headers(
      Authorization = paste("Bearer", token)
    ),
    encode = "json"
  )
  httr::stop_for_status(req)
}
