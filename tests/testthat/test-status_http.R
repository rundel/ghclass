test_that("API requests redraw the display while they wait", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE, cli.progress_show_after = 0)
  expect_equal(status_httr2_options(), list())
  expect_null(status_httr_config()[["options"]][["noprogress"]])
  expect_true(status_curl_callback(c(0, 0), c(0, 0)))

  seen = NULL
  local_mocked_bindings(
    req_perform = function(req, mock, ...) {
      expect_null(mock)
      seen <<- req[["options"]]
      "resp"
    },
    .package = "httr2"
  )

  out = capture_status_output(
    with_progress(
      status_scope("Fetching", 2, {
        hook = status_httr2_options()[["httr2_mock"]]
        expect_type(hook, "closure")
        expect_equal(hook(httr2::request("https://example.com")), "resp")
        expect_false(seen[["noprogress"]])
        expect_true(seen[["xferinfofunction"]](c(0, 0), c(0, 0)))

        config = status_httr_config()
        expect_false(config[["options"]][["noprogress"]])
        expect_identical(config[["options"]][["progressfunction"]], status_curl_callback)

        status_msg(ok_result(), "Fetched one.")
        status_msg(ok_result(), "Fetched two.")
      })
    )
  )
  frames = display_frames(out, "Fetching")
  expect_match(frames[1], " 0/2 *\r$")
  expect_match(frames[2], " 1/2 | Fetched one.", fixed = TRUE)
  expect_equal(visible_lines(out), "v 2 of 2 succeeded")

  local_mocked_bindings(status_redraw = function(...) stop("boom"))
  expect_true(status_curl_callback(c(0, 0), c(0, 0)))
})

test_that("ghclass_api_v3_req() attaches the hook except for downloads", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE, cli.progress_show_after = 0)
  local_mocked_bindings(github_get_token = function(...) "token")
  local_mocked_bindings(
    gh = function(...) {
      message("gh noise")
      status_redraw(force = TRUE)
      list(mock = getOption("httr2_mock"))
    },
    .package = "gh"
  )

  expect_null(ghclass_api_v3_req("GET /user")[["mock"]])

  out = capture_status_output(
    with_progress(
      status_scope("Fetching", 2, {
        expect_type(ghclass_api_v3_req("GET /user")[["mock"]], "closure")
        expect_null(ghclass_api_v3_req("GET /user", .destfile = tempfile())[["mock"]])
      })
    )
  )
  expect_false(any(grepl("gh noise", out, fixed = TRUE)))
  expect_length(display_frames(out, "Fetching"), 2)
})

test_that("progress mode preserves existing httr2 mocks for API requests", {
  local_status_output()
  withr::local_options(cli.ansi = FALSE)
  local_mocked_bindings(github_get_token = function(...) "")
  local_mocked_bindings(
    req_perform1 = function(...) stop("Unexpected real HTTP request"),
    .package = "httr2"
  )

  methods = character()
  mock = function(req) {
    methods <<- c(methods, req[["method"]])
    httr2::response(
      200,
      headers = list(`content-type` = "application/json"),
      body = charToRaw('{"login":"mock-user"}')
    )
  }
  withr::local_options(list(httr2_mock = mock))

  for (dynamic in c(FALSE, TRUE)) {
    withr::local_options(cli.dynamic = dynamic)
    suppressMessages(with_progress(status_scope("Fetching", 2, {
      for (endpoint in c("GET /user", "POST /user/repos")) {
        res = ghclass_api_v3_req(endpoint)
        expect_equal(res[["login"]], "mock-user")
        expect_identical(getOption("httr2_mock"), mock)
      }
    })))
  }

  expect_equal(methods, c("GET", "POST", "GET", "POST"))
})

test_that("the progress hook bypasses itself without changing the global mock", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE)
  calls = 0L
  hook = NULL
  local_mocked_bindings(
    req_perform1 = function(...) {
      calls <<- calls + 1L
      expect_identical(getOption("httr2_mock"), hook)
      httr2::response(200, body = charToRaw("ok"))
    },
    .package = "httr2"
  )

  suppressMessages(with_progress(status_scope("Fetching", 2, {
    hook = status_httr2_options()[["httr2_mock"]]
    res = withr::with_options(
      list(httr2_mock = hook),
      httr2::req_perform(httr2::request("https://example.com"))
    )
    expect_equal(httr2::resp_status(res), 200)
  })))
  expect_equal(calls, 1L)
})

