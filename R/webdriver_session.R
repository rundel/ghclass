get_debug_level = function()
{
  "DEBUG"
}

start_phantom = function()
{
  #cat("Starting phantom ...\n")
  stop_phantom()

  phantom = run_phantomjs(debugLevel = get_debug_level(), timeout=30000)
  assign("phantom", value = phantom, envir = .ghclass)
}

start_session = function()
{
  #cat("Starting session ...\n")
  stop_session()

  phantom = get_phantom()

  session = Session$new(port = phantom$port)
  assign("session", value = session, envir = .ghclass)
}

stop_session = function()
{
  #cat("Stopping session ...\n")

  session = get("session", envir=.ghclass)
  if (!is.null(session))
    session$delete()

  assign("session", value=NULL, envir=.ghclass)
}

stop_phantom = function()
{
  #cat("Stopping phantom ...\n")
  stop_session()

  phantom = get("phantom", envir=.ghclass)
  if (!is.null(phantom))
    phantom$process$kill()

  assign("phantom", value=NULL, envir=.ghclass)
}

get_phantom = function()
{
  #cat("Getting phantom ...\n")
  phantom = get("phantom", envir=.ghclass)
  if (is.null(phantom))
    start_phantom()

  get("phantom", envir=.ghclass)
}

get_session = function()
{
  #cat("Getting session ...\n")
  session = get("session", envir=.ghclass)
  if (is.null(session))
    start_session()

  get("session", envir=.ghclass)
}



