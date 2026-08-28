test_that("GitHub URLs default to github.com", {
  withr::with_envvar(c(GITHUB_API_URL = NA), {
    expect_equal(github_host_url(), "https://github.com")
    expect_equal(github_api_url(), "https://api.github.com")
    expect_equal(github_graphql_url(), "https://api.github.com/graphql")
  })

  withr::with_envvar(c(GITHUB_API_URL = "https://api.github.com/"), {
    expect_equal(github_host_url(), "https://github.com")
    expect_equal(github_api_url(), "https://api.github.com")
    expect_equal(github_graphql_url(), "https://api.github.com/graphql")
  })
})

test_that("GitHub URLs follow GITHUB_API_URL for Enterprise hosts", {
  withr::with_envvar(c(GITHUB_API_URL = "https://github.example.edu/api/v3"), {
    expect_equal(github_host_url(), "https://github.example.edu")
    expect_equal(github_api_url(), "https://github.example.edu/api/v3")
    expect_equal(github_graphql_url(), "https://github.example.edu/api/graphql")
  })

  withr::with_envvar(c(GITHUB_API_URL = "https://github.example.edu"), {
    expect_equal(github_host_url(), "https://github.example.edu")
    expect_equal(github_api_url(), "https://github.example.edu/api/v3")
    expect_equal(github_graphql_url(), "https://github.example.edu/api/graphql")
  })

  withr::with_envvar(c(GITHUB_API_URL = "http://ghe.local:8443/api/v3"), {
    expect_equal(github_host_url(), "http://ghe.local:8443")
    expect_equal(github_api_url(), "http://ghe.local:8443/api/v3")
    expect_equal(github_graphql_url(), "http://ghe.local:8443/api/graphql")
  })
})

test_that("Invalid GITHUB_API_URL values error", {
  withr::with_envvar(c(GITHUB_API_URL = "github.example.edu"), {
    expect_error(github_host_url(), "GITHUB_API_URL")
  })
})
