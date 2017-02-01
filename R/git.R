check_git = function()
{
  path <- Sys.which( "git" )

  return(path != "")
}

require_git = function()
{
  if (!check_git)
  {
    message(
      "git not found, if it is installed, please make sure the git executable ",
      "can be found via the PATH variable."
    )

    stop()
  }
}
