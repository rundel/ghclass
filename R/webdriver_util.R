set_element = function(css, value)
{
  session = get_session()

  elem = session$findElement(css=css)
  elem$clear()
  elem$setValue(value)
}

click_element = function(css)
{
  session = get_session()

  elem = session$findElement(css=css)
  elem$click()
}

wait_for_element = function(css, timeout=15000)
{
  session = get_session()

  js = paste0("document.querySelector('", css, "') !== null")
  if (!session$waitFor(js, timeout=timeout))
    stop("Unable to locate ", css, " within ", timeout/1000, " seconds.")
}
