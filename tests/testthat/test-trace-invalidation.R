context("trace-invalidation")

library(shiny)
options(shiny.reactlog = TRUE)

test_that("traceInvalidation does not show initial invalidation", {
  obs <- observe({})
  stack <- expect_silent(traceInvalidation(obs))
  expect_null(stack)
})

test_that("traceInvalidation handles contexts that have not been invalidated", {
  rx <- reactive({})
  isolate(rx())
  stack <- expect_silent(traceInvalidation(rx))
  expect_null(stack)
})

test_that("traceInvalidation works when a single dependency changes", {
  val <- reactiveVal(1)
  obs <- observe({
    val()
  })

  shiny:::flushReact()
  expect_silent(traceInvalidation(obs))

  val(2)
  valNode <- getValueNode(val)
  obsNode <- getContextNode(obs)

  stack <- expect_output(traceInvalidation(obs))

  expect_length(stack, 2)
  expect_identical(stack[[1]], obsNode)
  expect_identical(stack[[2]], valNode$prevNode)
})

test_that("traceInvalidation works within a reactive context", {
  val <- reactiveVal(1)
  obs <- observe({
    reactStack <<- traceInvalidation()
    val()
  })

  shiny:::flushReact()
  expect_silent(reactStack)

  val(2)
  valNode <- getValueNode(val)
  obsNode <- getContextNode(obs)

  expect_output(shiny:::flushReact())
  expect_length(reactStack, 2)
  expect_identical(reactStack[[1]], obsNode)
  expect_identical(reactStack[[2]], valNode$prevNode)
})

test_that("traceInvalidation traces reactive contexts setting values", {
  values <- reactiveValues(A = 1, B = 1)

  obsA <- observe({
    values$A
  })

  obsB <- observe({
    values$A <- values$B
  })

  shiny:::flushReact()
  isolate(values$B <- 2)
  shiny:::flushReact()

  stack <- expect_output(traceInvalidation(obsA))

  expect_length(stack, 5)
  expect_identical(stack[[2]], getValueNode(values, "A")$prevNode)
  expect_identical(stack[[3]], getContextNode(obsB)$prevNode)
  expect_identical(stack[[4]], getValueNode(values, "B")$prevNode)
  expect_identical(stack[[5]]$type, "isolate")
})

test_that("traceInvalidation shows the context where isolate was called", {
  values <- reactiveValues(A = 1)

  obsA <- observe({
    values$A
  })

  obsB <- observe({
    isolate({
      values$A <- 2
    })
  })

  shiny:::flushReact()

  stack <- expect_output(traceInvalidation(obsA))

  expect_length(stack, 4)
  expect_identical(stack[[3]]$type, "isolate")
  expect_identical(stack[[4]], getContextNode(obsB))

  isolate({
    isolate({
      values$A <- 3
    })
  })

  stack <- expect_output(traceInvalidation(obsA))

  expect_length(stack, 4)
  expect_equal(stack[[3]]$type, "isolate")
  expect_equal(stack[[4]]$type, "isolate")
})

test_that("traceInvalidation works when multiple dependencies change", {
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
  values$B <- 4
  values$A <- 2
  valNodeB <- getValueNode(values, "B")
  rxNode <- getContextNode(rx)

  obsStack <- expect_output(traceInvalidation(obs))
  rxStack <- expect_output(traceInvalidation(rx))
  expect_identical(obsStack[[2]], valNodeB$prevNode)
  expect_identical(rxStack[[2]], valNodeB$prevNode)

  shiny:::flushReact()
  values$C <- 6
  valNodeC <- getValueNode(values, "C")
  rxNode <- getContextNode(rx)

  obsStack <- expect_output(traceInvalidation(obs))
  rxStack <- expect_output(traceInvalidation(rx))
  expect_identical(obsStack[[2]], rxNode)
  expect_identical(obsStack[[3]], valNodeC$prevNode)
  expect_identical(rxStack[[2]], valNodeC$prevNode)
})

test_that("traceInvalidation works on a previously invalidated context", {
  valA <- reactiveVal(1)
  valB <- reactiveVal(1)

  obsA <- observe({
    valA()
  })

  obsB <- observe({
    valA(valB())
  })

  observe(valB(2))
  shiny:::flushReact()

  stack1 <- expect_output(traceInvalidation(obsA, n = 1))

  observe(valB(3))
  shiny:::flushReact()

  stack2 <- expect_output(traceInvalidation(obsA, n = 2))

  expect_length(stack1, 5)
  expect_identical(stack1, stack2)
})

test_that("traceInvalidation handles isolate expressions that set and get a reactive value", {
  val <- reactiveVal(1)

  obs <- observe({
    val()
  })

  shiny:::flushReact()

  isolate({
    val(val() + 1)
  })

  stack <- expect_output(traceInvalidation(obs))
  expect_length(stack, 3)
  expect_identical(stack[[2]], getValueNode(val)$prevNode)
  expect_identical(stack[[3]]$type, "isolate")
})
