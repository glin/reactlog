context("graph")

test_that("getReactGraph throws an error for a nonexistent session", {
  expect_error(getReactGraph(session = "not-a-session"))
})
