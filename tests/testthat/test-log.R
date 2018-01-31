context("log")

library(shiny)
options(shiny.reactlog = TRUE)

test_that("enableReactLog sets the `shiny.reactlog` option", {
  options(shiny.reactlog = NULL)
  enableReactLog()
  expect_true(getOption("shiny.reactlog"))
})

test_that("disableReactLog unsets the `shiny.reactlog` option", {
  options(shiny.reactlog = TRUE)
  disableReactLog()
  expect_null(getOption("shiny.reactlog"))
})

test_that("clearReactLog clears the reactive graph", {
  observe({})
  clearReactLog()
  graph <- getReactGraph()
  expect_length(graph$nodes, 0)
})
