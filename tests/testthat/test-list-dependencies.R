context("list-dependencies")

library(shiny)
options(shiny.reactlog = TRUE)

test_that("listDependencies handles reactives with no dependencies", {
  obs <- observe({})
  expect_silent(listDependencies(obs))
})

test_that("listDependencies lists a single dependency", {
  val <- reactiveVal(1)
  obs <- observe({
    val()
  })

  shiny:::flushReact()
  deps <- expect_output(listDependencies(obs))

  obsNode <- getContextNode(obs)
  expect_equal(deps$label, obsNode$label)

  expect_length(deps$children, 1)
  valNode <- getValueNode(val)
  expect_equal(deps$children[[1]]$label, valNode$label)
})

test_that("listDependencies lists multiple dependencies", {
  values <- reactiveValues(A = 1, B = 2, C = 3)

  rx <- reactive({
    values$A + values$B + values$C
  })

  obs <- observe({
    rx()
    values$A
    values$B
  })

  shiny:::flushReact()

  valNodeA <- getValueNode(values, "A")
  valNodeB <- getValueNode(values, "B")
  valNodeC <- getValueNode(values, "C")
  rxNode <- getContextNode(rx)

  rxDeps <- expect_output(listDependencies(rx))
  expect_length(rxDeps$children, 3)
  expect_equal(rxDeps$children[[1]]$label, valNodeA$label)
  expect_equal(rxDeps$children[[2]]$label, valNodeB$label)
  expect_equal(rxDeps$children[[3]]$label, valNodeC$label)

  obsDeps <- expect_output(listDependencies(obs))
  expect_length(obsDeps$children, 3)
  expect_equal(obsDeps$children[[1]], rxDeps)
  expect_equal(obsDeps$children[[2]]$label, valNodeA$label)
  expect_equal(obsDeps$children[[3]]$label, valNodeB$label)
})

test_that("listDependencies works with the last invalidated context", {
  val <- reactiveVal(1)
  obs <- observe({
    val()
  })

  deps <- expect_silent(listDependencies(obs, invalidated = TRUE))
  expect_equal(deps, expect_silent(listDependencies(obs)))

  shiny:::flushReact()
  val(2)
  shiny:::flushReact()

  obsNode <- getContextNode(obs)
  deps <- expect_output(listDependencies(obsNode, invalidated = TRUE))
  expect_equal(deps, expect_output(listDependencies(obsNode$prevNode)))
})

test_that("listDependencies handles contexts that have not been invalidated", {
  rx <- reactive({})
  isolate(rx())
  deps <- expect_silent(listDependencies(rx, invalidated = TRUE))
  expect_null(deps)
})

test_that("listDependencies handles contexts with no dependencies", {
  obs <- observe({})
  deps <- expect_silent(listDependencies(obs))
  expect_null(deps)

  isolate({
    deps <- expect_silent(listDependencies())
    expect_null(deps)
  })
})
