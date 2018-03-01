context("graph-nodes")

test_that("ValueNode formats label correctly when value is NULL", {
  valNode <- ValueNode$new("id")
  expect_equal(valNode$formatLabel(), "id")
  expect_equal(valNode$formatLabel(value = TRUE), "id")
})
