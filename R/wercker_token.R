get_wercker_token = function(quiet=FALSE)
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

  if (!quiet)
    stop("Unable to locate wercker token, please use set_wercker_token or
         define the WERCKER_TOKEN environmental variable.")
}

set_wercker_token = function(token)
{
  stopifnot(!missing(token))
  stopifnot(is.character(token))

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  assign("wercker_token", token, envir=.ghclass)
}

test_wercker_token = function(token)
{
  if (missing(token))
    token = get_wercker_token()

  # FIXME
  #test = try( gh("/", .token=token), silent = TRUE)

  return(!any(class(test) == "try-error"))
}
