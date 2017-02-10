get_wercker_account = function()
{
  account = get("wercker", envir=.ghclass)
  if (!is.null(account))
    return(account)

  if (file.exists("~/.wercker/account"))
  {
    set_wercker_account("~/.wercker/account")
    return(get_wercker_account())
  }

  stop("Unable to locate wercker account information, please use set_wercker_account")
}


set_wercker_account = function(account)
{
  stopifnot(!missing(account))

  if (is.character(account) & file.exists(account))
    account = yaml::yaml.load_file("~/.wercker/account")

  expected_names = c("username","password")

  if (is.null(names(account)))
    names(account) = expected_names
  names(account) = tolower(names(account))

  stopifnot(is.list(account))
  stopifnot(length(account)==2)
  stopifnot(!is.null(account$username))
  stopifnot(!is.null(account$password))

  assign("wercker", account, envir=.ghclass)
}


test_wercker_account = function(account)
{
  if (missing(account))
    account = get_wercker_account()

  #test = try( gh("/", .token=token), silent = TRUE)

  return(!any(class(test) == "try-error"))
}



wercker_login = function(account, debug=FALSE)
{
  if (missing(account))
    account = get_wercker_account()

  if (debug)
    cat("Logging into wercker ...\n")


  session = get_session()
  session$go("https://app.wercker.com/sessions/new/")
  set_element("#username", account$username)
  set_element("#password", account$password)
  click_element("#login")

  Sys.sleep(1)

  if (session$getUrl() == "https://app.wercker.com/sessions/new/") {
 #   session$takeScreenshot()
    stop("Unable to login to wercker")
  }

  if (debug)
    cat("Successfully logged into wercker.\n")
}

wercker_logged_in = function()
{
  session = get_session()
  session$go("https://app.wercker.com/")

  !detect_element('a.navbar-item[title="Log in"]')
}
