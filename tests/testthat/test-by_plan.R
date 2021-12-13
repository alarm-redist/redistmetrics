test_that("by_plan works", {

  x <- by_plan(letters)
  expect_equal(length(x), 26)

  x <- by_plan(rep(letters, each = 2))
  expect_equal(length(x), 26)

  x <- by_plan(rep(letters, 2))
  expect_equal(length(x), 52)
})
