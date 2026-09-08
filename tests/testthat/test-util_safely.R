test_that("error_msg_tree() handles errors without details", {
  res = purrr::safely(stop)("plain failure")

  tree = error_msg_tree(error_msg(res))

  expect_length(tree, 1)
  expect_match(tree, "plain failure")
})
