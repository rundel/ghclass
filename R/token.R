get_github_token = function(quiet=FALSE)
{
  if (exists("token", envir=.ghclass))
    return(get("token", envir=.ghclass))

  token = Sys.getenv("GITHUB_TOKEN")
  if (token != "")
  {
    assign("token", token, envir=.ghclass)
    return(token)
  }

  if (file.exists("~/.github/token"))
  {
    set_github_token(file="~/.github/token")
    return(get_github_token())
  }

  if (!quiet)
    stop("Unable to locate github token, please use set_github_token or
          define the GITHUB_TOKEN environmental variable.")
}

set_github_token = function(token=NULL, file)
{
  if (!missing(file))
    token = readLines(file, warn=FALSE)

  if(!is.null(token))
    assign("token", token, envir=.ghclass)
}

test_github_token = function(token=NULL)
{
  if (is.null(token))
    token = get_github_token()

  test = try( gh("/", .token=token), silent = TRUE)

  return(!any(class(test) == "try-error"))
}
