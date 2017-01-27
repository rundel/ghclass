get_github_token = function(quiet=FALSE)
{
  token = get("token", envir=.ghclass)
  if (!is.null(token))
    return(token)

  token = Sys.getenv("GITHUB_TOKEN")
  if (token != "")
  {
    assign("token", token, envir=.ghclass)
    return(token)
  }

  if (file.exists("~/.github/token"))
  {
    set_github_token("~/.github/token")
    return(get_github_token())
  }

  if (!quiet)
    stop("Unable to locate github token, please use set_github_token or
          define the GITHUB_TOKEN environmental variable.")
}

set_github_token = function(token)
{
  stopifnot(!missing(token))
  stopifnot(is.character(token))

  if (file.exists(token))
    token = readLines(file, warn=FALSE)

  assign("token", token, envir=.ghclass)
}

test_github_token = function(token)
{
  if (missing(token))
    token = get_github_token()

  test = try( gh("/", .token=token), silent = TRUE)

  return(!any(class(test) == "try-error"))
}
