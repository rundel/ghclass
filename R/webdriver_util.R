set_element = function(css, value)
{
  session = get_session()

  elem = session$findElement(css=css)
  elem$clear()
  elem$setValue(value)
}

get_element = function(css)
{
  session = get_session()

  elem = session$findElement(css=css)
  elem$getValue()
}



click_element = function(css)
{
  session = get_session()

  elem = session$findElement(css=css)
  elem$click()
}

wait_for_element = function(css, timeout=30000)
{
  session = get_session()

  js = paste0("document.querySelector('", css, "') !== null")
  if (!session$waitFor(js, timeout=timeout))
    stop("Unable to locate ", css, " within ", timeout/1000, " seconds.")
}

detect_element = function(css)
{
  session = get_session()

  node = try(session$findElement(css=css), silent = TRUE)

  !inherits(node, "try-error")
}

