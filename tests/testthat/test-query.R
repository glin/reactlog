context("query")

library(shiny)
options(shiny.reactlog = TRUE)

test_that("getContextNode finds the context node for an observer", {
  obs <- observe({})
  node <- getContextNode(obs)
  expect_true(is.ContextNode(node))
})

test_that("getContextNode finds the context node for a reactive expression", {
  rx <- reactive({})
  observe({
    rx()
  })

  shiny:::flushReact()

  node <- getContextNode(rx)
  expect_true(is.ContextNode(node))
})

test_that("getContextNode finds the context node for the current context", {
  obs <- observe({
    node <<- getContextNode()
  })

  shiny:::flushReact()

  expect_true(is.ContextNode(node))
  expect_identical(node, getContextNode(obs))
})

test_that("getContextNode finds the last invalidated context node", {
  val <- reactiveVal(1)
  rx <- reactive({
    val()
  })
  obs <- observe({
    rx()
  })

  expect_identical(
    getContextNode(obs),
    getContextNode(obs, invalidated = TRUE)
  )

  shiny:::flushReact()

  expect_null(getContextNode(rx, invalidated = TRUE))
  expect_identical(
    getContextNode(obs, invalidated = TRUE),
    getContextNode(obs)$prevNode
  )
})

test_that("getContextNode finds context nodes by ID", {
  obs <- observe({})
  node <- getContextNode(obs)
  expect_identical(getContextNode(node$id), node)
})

test_that("getContextNode works on context nodes", {
  obs <- observe({})
  node <- getContextNode(obs)
  expect_identical(getContextNode(node), node)
  expect_identical(getContextNode(node, invalidated = TRUE), node)
})

test_that("getContextNode returns NULL for unevaluated reactive expressions", {
  rx <- reactive({})
  node <- getContextNode(rx)
  expect_null(node)
})

test_that("getContextNode errors on invalid inputs", {
  expect_error(getContextNode(1))
  expect_error(getContextNode("x"))
  expect_error({
    val <- reactiveVal(1)
    getContextNode(val)
  })
})

test_that("getContextNode errors when the reactive log is disabled", {
  on.exit(options(shiny.reactlog = TRUE))
  options(shiny.reactlog = NULL)
  obs <- observe({})
  expect_error(getContextNode(obs))
})

test_that("getValueNode finds the value node for a reactive value", {
  val <- reactiveVal(1)
  node <- getValueNode(val)
  expect_true(is.ValueNode(node))
})

test_that("getValueNode finds the value nodes for a reactive values object", {
  values <- reactiveValues(A = 1, B = 2)
  nodeA <- getValueNode(values, "A")
  nodeB <- getValueNode(values, "B")
  expect_true(is.ValueNode(nodeA))
  expect_true(is.ValueNode(nodeB))
})

test_that("getValueNode finds value nodes by ID", {
  val <- reactiveVal(1)
  node <- getValueNode(val)
  expect_identical(getValueNode(node$id), node)
})

test_that("getValueNode works when given a value node as input", {
  val <- reactiveVal(1)
  node <- getValueNode(val)
  expect_identical(getValueNode(node), node)
})

test_that("getValueNode errors on invalid inputs", {
  expect_error(getValueNode(NULL))
  expect_error(getValueNode("x"))
  expect_error({
    obs <- observe({})
    getValueNode(obs)
  })
})

test_that("getValueNode errors on missing reactive values", {
  values <- reactiveValues(A = 1, B = 2)
  expect_error(getValueNode(values, "C"))
})

test_that("getValueNode errors when the reactive log is disabled", {
  on.exit(options(shiny.reactlog = TRUE))
  options(shiny.reactlog = NULL)
  clearReactLog()
  val <- reactiveVal()
  expect_error(getValueNode(val))
})
