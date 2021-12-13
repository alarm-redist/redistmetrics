test_that("prep_perims works", {
  x <- prep_perims(nh)
  expect_equal(sum(x$edge), 38135377, tolerance = 1e-4)
  expect_equal(nrow(x), 1833)
})
